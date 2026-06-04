-- | Desugars a module from AST to CoreFn representation.
module Language.PureScript.CoreFn.Desugar
  ( moduleToCoreFn
  ) where

import Prelude

import Data.Array as Array
import Data.Array (mapMaybe, concatMap)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.List.NonEmpty as NEL
import Data.String (Pattern(..), stripSuffix)
import Data.Tuple (Tuple(..), fst, snd, swap)

import Language.PureScript.AST.Declarations
  ( CaseAlternative(..)
  , DataConstructorDeclaration(..)
  , Declaration(..)
  , DeclarationRef(..)
  , ExportSource(..)
  , Expr(..)
  , GuardedExpr(..)
  , Guard(..)
  , Module(..)
  , WhereProvenance(..)
  , ValueDeclarationData(..)
  )
import Language.PureScript.AST.Binders (Binder(..)) as A
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (SourceSpan(..), SourcePos(..))
import Language.PureScript.Comments (Comment)
import Language.PureScript.CoreFn.Ann (Ann, ssAnn)
import Language.PureScript.CoreFn.Binders (Binder(..))
import Language.PureScript.CoreFn.Expr (Bind(..), CaseAlternative(..), Expr(..), Guard) as CoreFn
import Language.PureScript.CoreFn.Meta (ConstructorType(..), Meta(..))
import Language.PureScript.CoreFn.Module (Module(..)) as CoreFn
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (DataDeclType(..), Environment(..), NameKind(..))
import Language.PureScript.Label (Label(..))
import Language.PureScript.Names
  ( ConstructorName
  , Ident(..)
  , ModuleName(..)
  , ProperName(..)
  , Qualified(..)
  , QualifiedBy(..)
  , TypeName
  , byNullSourcePos
  , getQual
  , runProperName
  )
import Language.PureScript.PSString (PSString)
import Language.PureScript.Types (SourceType, Type(..))
import Language.PureScript.AST.Traversals (everythingOnValues)
import Language.PureScript.Constants.Prim as C

-- ---------------------------------------------------------------------------
-- Helpers exported from this module (used only internally)

-- | Look up a data constructor in the environment (partial).
lookupConstructor
  :: Environment
  -> Qualified (ProperName ConstructorName)
  -> Tuple (Tuple (Tuple DataDeclType (ProperName TypeName)) SourceType) (Array Ident)
lookupConstructor (Environment env) ctor =
  case Map.lookup ctor env.dataConstructors of
    Just x  -> x
    Nothing -> internalError ("Data constructor not found: " <> show ctor <> "; available keys: " <> show (Map.keys env.dataConstructors))

-- | Returns true when the ProperName ends with "$Dict".
isDictTypeName :: forall a. ProperName a -> Boolean
isDictTypeName (ProperName s) = isJust (stripSuffix (Pattern "$Dict") s)

-- | Returns the file-path portion of a SourceSpan.
spanName :: SourceSpan -> String
spanName (SourceSpan ss) = ss.name

-- | Check if a SourceSpan is the null span.
isNullSourceSpan :: SourceSpan -> Boolean
isNullSourceSpan (SourceSpan ss) =
  ss.name == ""
  && ss.start == SourcePos { line: 0, column: 0 }
  && ss.end   == SourcePos { line: 0, column: 0 }

-- | `Prim.undefined` identifier.
undefinedQualIdent :: Qualified Ident
undefinedQualIdent = Qualified (ByModuleName (ModuleName "Prim")) (Ident "undefined")

-- | Converts a ProperName to an Ident.
properToIdent :: forall a. ProperName a -> Ident
properToIdent = Ident <<< runProperName

-- ---------------------------------------------------------------------------
-- moduleToCoreFn

-- | Desugars a module from AST to CoreFn representation.
moduleToCoreFn :: Environment -> Module -> CoreFn.Module Ann
moduleToCoreFn _ (Module _ _ _ _ Nothing) =
  internalError "Module exports were not elaborated before moduleToCoreFn"
moduleToCoreFn env (Module modSS coms mn decls (Just exps)) =
  let
    imports  = mapMaybe importToCoreFn decls
                 <> map (Tuple (ssAnn modSS)) (findQualModules decls)
    imports' = dedupeImports imports
    exps'    = Array.nub (concatMap exportToCoreFn exps)
    -- reExps: Map ModuleName (Array Ident), merging lists from re-exports
    reExpMaps = mapMaybe (map reExportsToCoreFn <<< toReExportRef) exps
    reExps   = map Array.nub (foldl (Map.unionWith (<>)) Map.empty reExpMaps)
    externs  = Array.nub (mapMaybe externToCoreFn decls)
    decls'   = concatMap declToCoreFn decls
  in
    CoreFn.Module
      { moduleSourceSpan: modSS
      , moduleComments:   coms
      , moduleName:       mn
      , modulePath:       spanName modSS
      , moduleImports:    imports'
      , moduleExports:    exps'
      , moduleReExports:  reExps
      , moduleForeign:    externs
      , moduleDecls:      decls'
      }
  where

  -- Creates a map from a module name to the re-export references in that module.
  reExportsToCoreFn :: Tuple ModuleName DeclarationRef -> Map.Map ModuleName (Array Ident)
  reExportsToCoreFn (Tuple mn' ref') = Map.singleton mn' (exportToCoreFn ref')

  toReExportRef :: DeclarationRef -> Maybe (Tuple ModuleName DeclarationRef)
  toReExportRef (ReExportRef _ (ExportSource src) ref) =
    map (\importedFrom -> Tuple importedFrom ref) src.exportSourceImportedFrom
  toReExportRef _ = Nothing

  -- Remove duplicate imports (keep first occurrence per ModuleName).
  dedupeImports :: Array (Tuple Ann ModuleName) -> Array (Tuple Ann ModuleName)
  dedupeImports xs =
    -- swap so ModuleName is the key, dedupe via Map, then swap back
    map swap
    <<< Map.toUnfoldable
    <<< Map.fromFoldableWith (\_ b -> b)
    <<< map swap
    $ xs

  -- Convenience: build an Ann with only source span.
  ssA :: SourceSpan -> Ann
  ssA ss = Tuple ss (Tuple [] Nothing)

  -- -------------------------------------------------------------------------
  -- declToCoreFn

  declToCoreFn :: Declaration -> Array (CoreFn.Bind Ann)
  declToCoreFn (DataDeclaration (Tuple ss com) Newtype _ _ ctors) =
    case ctors of
      [DataConstructorDeclaration ctor] ->
        let ctorName = ctor.dataCtorName
            meta     = if isDictTypeName ctorName then Just IsTypeClassConstructor else Nothing
        in [ CoreFn.NonRec
               (Tuple ss (Tuple [] meta))
               (properToIdent ctorName)
               (CoreFn.Abs
                 (Tuple ss (Tuple com (Just IsNewtype)))
                 (Ident "x")
                 (CoreFn.Var (ssAnn ss) (Qualified byNullSourcePos (Ident "x"))))
           ]
      _ -> internalError "Found newtype with multiple constructors in declToCoreFn"

  declToCoreFn (DataDeclaration (Tuple ss _com) Data tyName _ ctors) =
    ctors # map \(DataConstructorDeclaration ctorDecl) ->
      let ctor   = ctorDecl.dataCtorName
          fields = snd (lookupConstructor env (Qualified (ByModuleName mn) ctor))
      in CoreFn.NonRec
           (ssA ss)
           (properToIdent ctor)
           (CoreFn.Constructor (Tuple ss (Tuple [] Nothing)) tyName ctor fields)

  declToCoreFn (DataBindingGroupDeclaration ds) =
    concatMap declToCoreFn (NEL.toUnfoldable ds)

  declToCoreFn (ValueDeclaration (ValueDeclarationData vd)) =
    case vd.valdeclExpression of
      [GuardedExpr [] e] ->
        let ss  = fst vd.valdeclSourceAnn
            com = snd vd.valdeclSourceAnn
        in [ CoreFn.NonRec (ssA ss) vd.valdeclIdent (exprToCoreFn ss com Nothing e) ]
      _ -> []

  declToCoreFn (BindingGroupDeclaration ds) =
    let items = NEL.toUnfoldable ds
                  :: Array (Tuple (Tuple (Tuple SourceSpan (Array Comment)) Ident)
                                  (Tuple NameKind Expr))
    in [ CoreFn.Rec $ items # map \(Tuple (Tuple (Tuple ss com) name) (Tuple _nk e)) ->
           Tuple (Tuple (ssA ss) name) (exprToCoreFn ss com Nothing e)
       ]

  declToCoreFn _ = []

  -- -------------------------------------------------------------------------
  -- exprToCoreFn

  exprToCoreFn :: SourceSpan -> Array Comment -> Maybe SourceType -> Expr -> CoreFn.Expr Ann
  exprToCoreFn _ com _ (Literal ss lit) =
    CoreFn.Literal (Tuple ss (Tuple com Nothing)) (map (exprToCoreFn ss com Nothing) lit)

  exprToCoreFn ss com _ (Accessor name v) =
    CoreFn.Accessor (Tuple ss (Tuple com Nothing)) name (exprToCoreFn ss [] Nothing v)

  exprToCoreFn ss com ty (ObjectUpdate obj vs) =
    let obj' = exprToCoreFn ss [] Nothing obj
        vs'  = map (\(Tuple k v) -> Tuple k (exprToCoreFn ss [] Nothing v)) vs
        unchanged = ty >>= unchangedRecordFields (map fst vs)
    in CoreFn.ObjectUpdate (Tuple ss (Tuple com Nothing)) obj' unchanged vs'
    where
    unchangedRecordFields :: Array PSString -> SourceType -> Maybe (Array PSString)
    unchangedRecordFields updated (TypeApp _ (TypeConstructor _ rc) row)
      | rc == C.tyRecord = collectRow updated row
    unchangedRecordFields _ _ = Nothing

    collectRow :: Array PSString -> SourceType -> Maybe (Array PSString)
    collectRow _ ty' | isREmptyKinded ty' = Just []
    collectRow updated' (RCons _ (Label l) _ r) =
      map (\rest -> if Array.elem l updated' then rest else Array.cons l rest) (collectRow updated' r)
    collectRow _ _ = Nothing

    isREmptyKinded :: forall a. Type a -> Boolean
    isREmptyKinded (REmpty _)               = true
    isREmptyKinded (KindApp _ (REmpty _) _) = true
    isREmptyKinded _                        = false

  exprToCoreFn ss com _ (Abs (A.VarBinder _ name) v) =
    CoreFn.Abs (Tuple ss (Tuple com Nothing)) name (exprToCoreFn ss [] Nothing v)

  exprToCoreFn _ _ _ (Abs _ _) =
    internalError "Abs with non-VarBinder argument was not desugared before exprToCoreFn"

  exprToCoreFn ss com _ (App v1 v2) =
    let v1'  = exprToCoreFn ss [] Nothing v1
        v2'  = exprToCoreFn ss [] Nothing v2
        meta = if isDictCtor v1 || isSynthetic v2
               then Just IsSynthetic
               else Nothing
    in CoreFn.App (Tuple ss (Tuple com meta)) v1' v2'
    where
    isDictCtor :: Expr -> Boolean
    isDictCtor (Constructor _ (Qualified _ name)) = isDictTypeName name
    isDictCtor _ = false

    isSynthetic :: Expr -> Boolean
    isSynthetic (App v3 v4)     = isDictCtor v3 || (isSynthetic v3 && isSynthetic v4)
    isSynthetic (Accessor _ v3) = isSynthetic v3
    isSynthetic (Var ss' _)     = isNullSourceSpan ss'
    isSynthetic (Unused _)      = true
    isSynthetic _               = false

  exprToCoreFn ss com _ (Unused _) =
    CoreFn.Var (Tuple ss (Tuple com Nothing)) undefinedQualIdent

  exprToCoreFn _ com _ (Var ss ident) =
    CoreFn.Var (Tuple ss (Tuple com (getValueMeta ident))) ident

  exprToCoreFn ss com _ (IfThenElse v1 v2 v3) =
    CoreFn.Case (Tuple ss (Tuple com Nothing))
      [ exprToCoreFn ss [] Nothing v1 ]
      [ CoreFn.CaseAlternative
          { caseAlternativeBinders: [ LiteralBinder (ssAnn ss) (BooleanLiteral true) ]
          , caseAlternativeResult:  Right (exprToCoreFn ss [] Nothing v2)
          }
      , CoreFn.CaseAlternative
          { caseAlternativeBinders: [ NullBinder (ssAnn ss) ]
          , caseAlternativeResult:  Right (exprToCoreFn ss [] Nothing v3)
          }
      ]

  exprToCoreFn _ com _ (Constructor ss name) =
    CoreFn.Var (Tuple ss (Tuple com (Just (getConstructorMeta name)))) (map properToIdent name)

  exprToCoreFn ss com _ (Case vs alts) =
    CoreFn.Case (Tuple ss (Tuple com Nothing))
      (map (exprToCoreFn ss [] Nothing) vs)
      (map (altToCoreFn ss) alts)

  exprToCoreFn ss com _ (TypedValue _ v ty) =
    exprToCoreFn ss com (Just ty) v

  exprToCoreFn ss com _ (Let w ds v) =
    CoreFn.Let (Tuple ss (Tuple com (getLetMeta w)))
      (concatMap declToCoreFn ds)
      (exprToCoreFn ss [] Nothing v)

  exprToCoreFn _ com ty (PositionedValue ss com1 v) =
    exprToCoreFn ss (com <> com1) ty v

  exprToCoreFn _ _ _ e =
    internalError ("Unexpected value in exprToCoreFn: " <> show e)

  -- -------------------------------------------------------------------------
  -- altToCoreFn

  altToCoreFn :: SourceSpan -> CaseAlternative -> CoreFn.CaseAlternative Ann
  altToCoreFn ss (CaseAlternative ca) =
    CoreFn.CaseAlternative
      { caseAlternativeBinders: map (binderToCoreFn ss []) ca.caseAlternativeBinders
      , caseAlternativeResult:  go ca.caseAlternativeResult
      }
    where
    go :: Array GuardedExpr
       -> Either (Array (Tuple (CoreFn.Guard Ann) (CoreFn.Expr Ann))) (CoreFn.Expr Ann)
    go [GuardedExpr [] e] = Right (exprToCoreFn ss [] Nothing e)
    go gs =
      Left $ gs >>= \(GuardedExpr g e) ->
        [ Tuple (exprToCoreFn ss [] Nothing (guardToExpr g))
                (exprToCoreFn ss [] Nothing e)
        ]

    guardToExpr :: Array Guard -> Expr
    guardToExpr [ConditionGuard cond] = cond
    guardToExpr _ = internalError "Guard not correctly desugared"

  -- -------------------------------------------------------------------------
  -- binderToCoreFn

  binderToCoreFn :: SourceSpan -> Array Comment -> A.Binder -> Binder Ann
  binderToCoreFn _ com (A.LiteralBinder ss lit) =
    LiteralBinder (Tuple ss (Tuple com Nothing)) (map (binderToCoreFn ss com) lit)

  binderToCoreFn ss com A.NullBinder =
    NullBinder (Tuple ss (Tuple com Nothing))

  binderToCoreFn _ com (A.VarBinder ss name) =
    VarBinder (Tuple ss (Tuple com Nothing)) name

  binderToCoreFn _ com (A.ConstructorBinder ss dctor@(Qualified mn' _) bs) =
    let lookupR  = lookupConstructor env dctor
        tyCtorName = snd (fst (fst lookupR))
    in ConstructorBinder
         (Tuple ss (Tuple com (Just (getConstructorMeta dctor))))
         (Qualified mn' tyCtorName)
         dctor
         (map (binderToCoreFn ss []) bs)

  binderToCoreFn _ com (A.NamedBinder ss name b) =
    NamedBinder (Tuple ss (Tuple com Nothing)) name (binderToCoreFn ss [] b)

  binderToCoreFn _ com (A.PositionedBinder ss com1 b) =
    binderToCoreFn ss (com <> com1) b

  binderToCoreFn ss com (A.TypedBinder _ b) =
    binderToCoreFn ss com b

  binderToCoreFn _ _ (A.OpBinder _ _) =
    internalError "OpBinder should have been desugared before binderToCoreFn"

  binderToCoreFn _ _ (A.BinaryNoParensBinder _ _ _) =
    internalError "BinaryNoParensBinder should have been desugared before binderToCoreFn"

  binderToCoreFn _ _ (A.ParensInBinder _) =
    internalError "ParensInBinder should have been desugared before binderToCoreFn"

  -- -------------------------------------------------------------------------
  -- Meta helpers

  getLetMeta :: WhereProvenance -> Maybe Meta
  getLetMeta FromWhere = Just IsWhere
  getLetMeta FromLet   = Nothing

  getValueMeta :: Qualified Ident -> Maybe Meta
  getValueMeta name =
    let Environment envR = env
    in case Map.lookup name envR.names of
         Just (Tuple (Tuple _ External) _) -> Just IsForeign
         _ -> Nothing

  getConstructorMeta :: Qualified (ProperName ConstructorName) -> Meta
  getConstructorMeta ctor =
    let lookupR = lookupConstructor env ctor
        dt      = fst (fst (fst lookupR))
        fields  = snd lookupR
    in case dt of
         Newtype -> IsNewtype
         Data    ->
           let constructorType = if numConstructors ctor == 1 then ProductType else SumType
           in IsConstructor constructorType fields

  numConstructors :: Qualified (ProperName ConstructorName) -> Int
  numConstructors ctor =
    let Environment envR = env
        tc = typeConstructorOf ctor (lookupConstructor env ctor)
        allCtors = Map.toUnfoldable envR.dataConstructors
                     :: Array (Tuple (Qualified (ProperName ConstructorName))
                                     (Tuple (Tuple (Tuple DataDeclType (ProperName TypeName)) SourceType) (Array Ident)))
    in Array.length $ Array.filter (\(Tuple k v) -> typeConstructorOf k v == tc) allCtors

  typeConstructorOf
    :: Qualified (ProperName ConstructorName)
    -> Tuple (Tuple (Tuple DataDeclType (ProperName TypeName)) SourceType) (Array Ident)
    -> Tuple ModuleName (ProperName TypeName)
  typeConstructorOf (Qualified (ByModuleName mn') _) lookupR =
    Tuple mn' (snd (fst (fst lookupR)))
  typeConstructorOf _ _ =
    internalError "Invalid argument to typeConstructorOf"

-- ---------------------------------------------------------------------------
-- findQualModules

-- | Find module names from qualified references to values.
findQualModules :: Array Declaration -> Array ModuleName
findQualModules decls =
  let t = everythingOnValues (<>) fqDecls fqValues fqBinders (const []) (const [])
  in concatMap t.decl decls
  where
  fqDecls :: Declaration -> Array ModuleName
  fqDecls (TypeInstanceDeclaration _ _ _ _ _ _ q _ _) = getQual' q
  fqDecls _ = []

  fqValues :: Expr -> Array ModuleName
  fqValues (Var _ q) = getQual' q
  fqValues (Constructor _ q) = getQual' q
  fqValues _ = []

  fqBinders :: A.Binder -> Array ModuleName
  fqBinders (A.ConstructorBinder _ q _) = getQual' q
  fqBinders _ = []

  getQual' :: forall a. Qualified a -> Array ModuleName
  getQual' q = case getQual q of
    Just mn' -> [mn']
    Nothing  -> []

-- ---------------------------------------------------------------------------
-- importToCoreFn

-- | Desugars import declarations from AST to CoreFn representation.
importToCoreFn :: Declaration -> Maybe (Tuple Ann ModuleName)
importToCoreFn (ImportDeclaration (Tuple ss com) name _ _) =
  Just (Tuple (Tuple ss (Tuple com Nothing)) name)
importToCoreFn _ = Nothing

-- ---------------------------------------------------------------------------
-- externToCoreFn

-- | Desugars foreign declarations from AST to CoreFn representation.
externToCoreFn :: Declaration -> Maybe Ident
externToCoreFn (ExternDeclaration _ name _) = Just name
externToCoreFn _ = Nothing

-- ---------------------------------------------------------------------------
-- exportToCoreFn

-- | Desugars export declarations references from AST to CoreFn representation.
exportToCoreFn :: DeclarationRef -> Array Ident
exportToCoreFn (TypeRef _ _ (Just dctors)) = map properToIdent dctors
exportToCoreFn (TypeRef _ _ Nothing)       = []
exportToCoreFn (TypeOpRef _ _)             = []
exportToCoreFn (ValueRef _ name)           = [name]
exportToCoreFn (ValueOpRef _ _)            = []
exportToCoreFn (TypeClassRef _ _)          = []
exportToCoreFn (TypeInstanceRef _ name _)  = [name]
exportToCoreFn (ModuleRef _ _)             = []
exportToCoreFn (ReExportRef _ _ _)         = []
