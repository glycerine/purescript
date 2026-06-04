module Language.PureScript.Sugar.BindingGroups
  ( createBindingGroups
  , createBindingGroupsModule
  , collapseBindingGroups
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Array as Array
import Data.Array (mapMaybe, nub, intersect, (\\))
import Data.Graph (SCC(..), stronglyConnComp, stronglyConnCompR)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd, swap)

import Language.PureScript.AST.Binders (Binder(..))
import Language.PureScript.AST.Declarations
  ( CaseAlternative(..)
  , Declaration(..)
  , Expr(..)
  , GuardedExpr(..)
  , Module(..)
  , RoleDeclarationData(..)
  , ValueDeclarationData(..)
  , WhereProvenance(..)
  , TypeInstanceBody(..)
  , declSourceSpan
  , isDataDecl
  , isExternDataDecl
  , isTypeSynonymDecl
  , isTypeClassDecl
  , isRoleDecl
  , isKindDecl
  , isImportDecl
  , isTypeClassInstanceDecl
  , isFixityDecl
  , isExternDecl
  , getValueDeclaration
  , traverseTypeInstanceBody
  )
import Language.PureScript.AST.SourcePos (SourceAnn, SourcePos(..), SourceSpan, nullSourceAnn)
import Language.PureScript.AST.Traversals
  ( ScopedIdent(..)
  , everythingOnValues
  , everythingWithContextOnValues
  , everythingWithScope
  , everywhereOnValues
  , everywhereOnValuesTopDownM
  , accumTypes
  )
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (NameKind(..))
import Language.PureScript.Errors
  ( ErrorMessage(..)
  , MultipleErrors(..)
  , SimpleErrorMessage(..)
  , addHint
  , errorMessage'
  , parU
  , positionedError
  , rethrow
  )
import Language.PureScript.Names
  ( Ident(..)
  , ModuleName
  , ProperName(..)
  , ProperNameType
  , Qualified(..)
  , QualifiedBy(..)
  , TypeName
  , coerceProperName
  )
import Language.PureScript.Types
  ( Constraint(..)
  , SourceConstraint
  , SourceType
  , Type(..)
  , everythingOnTypes
  )
import Language.PureScript.AST.Declarations (ErrorMessageHint(..))

data VertexType
  = VertexDefinition
  | VertexKindSignature
  | VertexRoleDeclaration

derive instance eqVertexType :: Eq VertexType
derive instance ordVertexType :: Ord VertexType

createBindingGroupsModule
  :: forall m
   . MonadError MultipleErrors m
  => Module
  -> m Module
createBindingGroupsModule (Module ss coms name ds exps) =
  Module ss coms name <$> createBindingGroups name ds <*> pure exps

createBindingGroups
  :: forall m
   . MonadError MultipleErrors m
  => ModuleName
  -> Array Declaration
  -> m (Array Declaration)
createBindingGroups moduleName ds = do
  ds' <- handleDecls ds
  traverse f ds'
  where
  t = everywhereOnValuesTopDownM pure handleExprs pure
  f = t.decl

  handleExprs :: Expr -> m Expr
  handleExprs (Let w letDs val) = (\ds' -> Let w ds' val) <$> handleDecls letDs
  handleExprs other = pure other

  handleDecls :: Array Declaration -> m (Array Declaration)
  handleDecls decls = do
    let values = mapMaybe (map (map extractGuardedExpr) <<< getValueDeclaration) decls
        kindDecls = map (\d -> Tuple d VertexKindSignature) (Array.filter isKindDecl decls)
        dataDecls = map (\d -> Tuple d VertexDefinition)
          (Array.filter (\a -> isDataDecl a || isExternDataDecl a || isTypeSynonymDecl a || isTypeClassDecl a) decls)
        roleDecls = map (\d -> Tuple d VertexRoleDeclaration) (Array.filter isRoleDecl decls)
        roleAnns = map (declTypeName <<< fst) roleDecls
        kindSigs = map (declTypeName <<< fst) kindDecls
        typeSyns = map declTypeName (Array.filter isTypeSynonymDecl decls)
        nonTypeSynKindSigs = kindSigs \\ typeSyns
        allDecls = kindDecls <> dataDecls <> roleDecls
        allProperNames = map (declTypeName <<< fst) allDecls

        mkVert (Tuple d vty) =
          let names = Array.intersect (usedTypeNames moduleName d) allProperNames
              dName = declTypeName d
              vtype n
                | vty == VertexKindSignature && Array.elem n nonTypeSynKindSigs = VertexKindSignature
                | otherwise = VertexDefinition
              deps = map (\n -> Tuple n (vtype n)) names
              self = case vty of
                VertexDefinition ->
                  (if Array.elem dName kindSigs then [Tuple dName VertexKindSignature] else [])
                  <> (if Array.elem dName roleAnns && not (isExternDataDecl d) then [Tuple dName VertexRoleDeclaration] else [])
                VertexRoleDeclaration -> [Tuple dName VertexDefinition]
                _ -> []
          in Tuple (Tuple d (Tuple dName vty)) (Tuple (Tuple dName vty) (self <> deps))

        dataVerts = map mkVert allDecls

    dataBindingGroupDecls <- parU (stronglyConnCompR dataVerts) toDataBindingGroup

    let makeKey (ValueDeclarationData vd) = Tuple (exprHasNoTypeHole (getValExpr (ValueDeclarationData vd))) vd.valdeclIdent
        valueDeclarationKeys = map makeKey values
        valueDeclarationInfo = Map.fromFoldable (map swap valueDeclarationKeys)
        findInfo i = Tuple (fromMaybe false (Map.lookup i valueDeclarationInfo)) i
        computeDeps vd = Array.intersect valueDeclarationKeys (map findInfo (usedIdents moduleName vd))

        makeVert vd = Tuple vd (Tuple (makeKey vd) (computeDeps vd))
        valueDeclarationVerts = map makeVert values

    bindingGroupDecls <- parU (stronglyConnComp valueDeclarationVerts) (toBindingGroup moduleName)

    pure $
      Array.filter isImportDecl decls
      <> dataBindingGroupDecls
      <> Array.filter isTypeClassInstanceDecl decls
      <> Array.filter isFixityDecl decls
      <> Array.filter isExternDecl decls
      <> bindingGroupDecls

  extractGuardedExpr :: Array GuardedExpr -> Expr
  extractGuardedExpr [GuardedExpr [] expr] = expr
  extractGuardedExpr _ = internalError "Expected Guards to have been desugared in handleDecls."

  getValExpr :: ValueDeclarationData Expr -> Expr
  getValExpr (ValueDeclarationData vd) = vd.valdeclExpression

  exprHasNoTypeHole :: Expr -> Boolean
  exprHasNoTypeHole = not <<< exprHasTypeHole
    where
    t2 = everythingOnValues (||) (const false) goExpr (const false) (const false) (const false)
    exprHasTypeHole e = t2.expr e

    goExpr :: Expr -> Boolean
    goExpr (Hole _) = true
    goExpr _ = false

collapseBindingGroups :: Array Declaration -> Array Declaration
collapseBindingGroups = map f <<< flattenBindingGroups
  where
  t = everywhereOnValues identity flattenBGForValue identity
  f = t.decl

flattenBGForValue :: Expr -> Expr
flattenBGForValue (Let w ds val) = Let w (flattenBindingGroups ds) val
flattenBGForValue other = other

flattenBindingGroups :: Array Declaration -> Array Declaration
flattenBindingGroups = Array.concatMap go
  where
  go (DataBindingGroupDeclaration ds) = NEL.toUnfoldable ds
  go (BindingGroupDeclaration ds) =
    NEL.toUnfoldable $ map (\(Tuple (Tuple sa ident) (Tuple nameKind val)) ->
      ValueDeclaration (ValueDeclarationData
        { valdeclSourceAnn: sa
        , valdeclIdent: ident
        , valdeclName: nameKind
        , valdeclBinders: []
        , valdeclExpression: [GuardedExpr [] val]
        })) ds
  go other = [other]

usedIdents :: ModuleName -> ValueDeclarationData Expr -> Array Ident
usedIdents moduleName (ValueDeclarationData vd) = ordNub (usedIdents' Set.empty vd.valdeclExpression)
  where
  t = everythingWithScope (const (const [])) usedNamesE (const (const [])) (const (const [])) (const (const []))

  usedIdents' scope expr = t.expr scope expr

  usedNamesE :: Set ScopedIdent -> Expr -> Array Ident
  usedNamesE scope (Var _ (Qualified (BySourcePos _) name))
    | not (Set.member (LocalIdent name) scope) = [name]
  usedNamesE scope (Var _ (Qualified (ByModuleName mn) name))
    | mn == moduleName && not (Set.member (ToplevelIdent name) scope) = [name]
  usedNamesE _ _ = []

usedImmediateIdents :: ModuleName -> Declaration -> Array Ident
usedImmediateIdents moduleName decl = ordNub (t.decl decl)
  where
  t = everythingWithContextOnValues true [] (<>) (const (\d -> Tuple true [])) usedNamesE (const (\b -> Tuple true [])) (const (\ca -> Tuple true [])) (const (\dn -> Tuple true []))

  usedNamesE :: Boolean -> Expr -> Tuple Boolean (Array Ident)
  usedNamesE inScope (Var _ (Qualified (BySourcePos _) name))
    | inScope = Tuple true [name]
  usedNamesE inScope (Var _ (Qualified (ByModuleName mn) name))
    | inScope && mn == moduleName = Tuple true [name]
  usedNamesE _ (Abs _ _) = Tuple false []
  usedNamesE scope _ = Tuple scope []

usedTypeNames :: ModuleName -> Declaration -> Array (ProperName TypeName)
usedTypeNames moduleName decl = ordNub (t.decl decl <> usedNamesForTypeClassDeps decl)
  where
  t = accumTypes (everythingOnTypes (<>) usedNames)

  usedNames :: SourceType -> Array (ProperName TypeName)
  usedNames (ConstrainedType _ (Constraint c) _) = usedConstraint (Constraint c)
  usedNames (TypeConstructor _ (Qualified (ByModuleName mn) name))
    | mn == moduleName = [name]
  usedNames _ = []

  usedConstraint :: SourceConstraint -> Array (ProperName TypeName)
  usedConstraint (Constraint c) = case c.constraintClass of
    Qualified (ByModuleName mn) name
      | mn == moduleName -> [coerceProperName name]
    _ -> []

  usedNamesForTypeClassDeps :: Declaration -> Array (ProperName TypeName)
  usedNamesForTypeClassDeps (TypeClassDeclaration _ _ _ deps _ _) = Array.concatMap usedConstraint deps
  usedNamesForTypeClassDeps _ = []

declTypeName :: Declaration -> ProperName TypeName
declTypeName (DataDeclaration _ _ pn _ _) = pn
declTypeName (ExternDataDeclaration _ pn _) = pn
declTypeName (TypeSynonymDeclaration _ pn _ _) = pn
declTypeName (TypeClassDeclaration _ pn _ _ _ _) = coerceProperName pn
declTypeName (KindDeclaration _ _ pn _) = pn
declTypeName (RoleDeclaration (RoleDeclarationData rd)) = rd.rdeclIdent
declTypeName _ = internalError "Expected DataDeclaration"

toBindingGroup
  :: forall m
   . MonadError MultipleErrors m
  => ModuleName
  -> SCC (ValueDeclarationData Expr)
  -> m Declaration
toBindingGroup _ (AcyclicSCC d) = pure (mkDeclaration d)
toBindingGroup moduleName (CyclicSCC ds') = do
  bindings <- parU (stronglyConnComp valueVerts) toBinding
  case NEL.fromFoldable bindings of
    Nothing -> throwError (errorMessage' nullSS (InternalCompilerError "toBindingGroup" "empty SCC"))
    Just nel -> pure (BindingGroupDeclaration nel)
  where
  nullSS = nullSourceSpan
  idents = map valdeclIdent ds'
  valueVerts = map (\d ->
    let di = valdeclIdent d
        deps = usedImmediateIdents moduleName (mkDeclaration d) `Array.intersect` idents
    in Tuple d (Tuple di deps)) ds'

  toBinding :: SCC (ValueDeclarationData Expr) -> m (Tuple (Tuple SourceAnn Ident) (Tuple NameKind Expr))
  toBinding (AcyclicSCC d) = pure (fromValueDecl d)
  toBinding (CyclicSCC cycleDs) = throwError (Array.foldl (<>) mempty (map cycleError cycleDs))

  cycleError :: ValueDeclarationData Expr -> MultipleErrors
  cycleError (ValueDeclarationData vd) = errorMessage' (fst vd.valdeclSourceAnn) (CycleInDeclaration vd.valdeclIdent)

  valdeclIdent :: ValueDeclarationData Expr -> Ident
  valdeclIdent (ValueDeclarationData vd) = vd.valdeclIdent

nullSourceSpan :: SourceSpan
nullSourceSpan = nullSourceAnn # fst

-- The full vertex type returned by stronglyConnCompR:
-- SCC (Tuple Declaration (Tuple (Tuple (ProperName TypeName) VertexType) (Array (Tuple (ProperName TypeName) VertexType))))
toDataBindingGroup
  :: forall m
   . MonadError MultipleErrors m
  => SCC (Tuple Declaration (Tuple (Tuple (ProperName TypeName) VertexType) (Array (Tuple (ProperName TypeName) VertexType))))
  -> m Declaration
toDataBindingGroup (AcyclicSCC (Tuple d _)) = pure d
toDataBindingGroup (CyclicSCC ds') =
  let kindDecls = Array.concatMap (kindDecl <<< getDecl) ds'
  in if not (Array.null kindDecls)
     then case Array.head kindDecls of
       Just (Tuple ss pn) ->
         let nullPos = SourcePos { line: 0, column: 0 }
         in throwError (errorMessage' ss (CycleInKindDeclaration (NEL.singleton (Qualified (BySourcePos nullPos) pn))))
       Nothing -> throwError mempty
     else if not (Array.null typeSynonymCycles)
       then throwError (MultipleErrors (map toTypeSynError typeSynonymCycles))
       else case NEL.fromFoldable (map getDecl ds') of
         Nothing -> throwError (errorMessage' (fst nullSourceAnn) (InternalCompilerError "toDataBindingGroup" "empty SCC"))
         Just nel -> pure (DataBindingGroupDeclaration nel)
  where
  kindDecl (KindDeclaration sa _ pn _) = [Tuple (fst sa) pn]
  kindDecl (ExternDataDeclaration sa pn _) = [Tuple (fst sa) pn]
  kindDecl _ = []

  getDecl (Tuple d _) = d
  getName (Tuple _ (Tuple (Tuple n _) _)) = n

  typeSynonymCycles =
    let synVerts = Array.filter (\(Tuple d _) -> isJust (isTypeSynonym d)) ds'
    in if Array.null synVerts then [] else [synVerts]

  toTypeSynError syns =
    case Array.head syns of
      Just (Tuple d _) ->
        let pos = positionedError (declSourceSpan d)
            names = map (\(Tuple _ (Tuple (Tuple pn _) _)) -> pn) syns
        in case NEL.fromFoldable names of
             Just nel -> ErrorMessage [pos] (CycleInTypeSynonym nel)
             Nothing -> ErrorMessage [] (InternalCompilerError "toDataBindingGroup" "empty synonym cycle")
      Nothing -> ErrorMessage [] (InternalCompilerError "toDataBindingGroup" "empty synonym cycle")

isTypeSynonym :: Declaration -> Maybe (ProperName TypeName)
isTypeSynonym (TypeSynonymDeclaration _ pn _ _) = Just pn
isTypeSynonym _ = Nothing

mkDeclaration :: ValueDeclarationData Expr -> Declaration
mkDeclaration (ValueDeclarationData vd) = ValueDeclaration (ValueDeclarationData vd { valdeclExpression = [GuardedExpr [] vd.valdeclExpression] })

fromValueDecl :: ValueDeclarationData Expr -> Tuple (Tuple SourceAnn Ident) (Tuple NameKind Expr)
fromValueDecl (ValueDeclarationData vd) = Tuple (Tuple vd.valdeclSourceAnn vd.valdeclIdent) (Tuple vd.valdeclName vd.valdeclExpression)

ordNub :: forall a. Ord a => Array a -> Array a
ordNub = Array.nub
