module Language.PureScript.Sugar.TypeClasses
  ( desugarTypeClasses
  , typeClassMemberName
  , superClassDictionaryNames
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.State (StateT, evalStateT, get, modify_)
import Control.Monad.Writer.Class (class MonadWriter)
import Control.Monad.Supply.Class (class MonadSupply, freshIdent)
import Data.Array as Array
import Data.Array (mapMaybe, partition)
import Data.Graph (SCC(..), stronglyConnComp)
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set as Set
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..), fst, snd)

import Language.PureScript.AST.Binders (Binder(..))
import Language.PureScript.AST.Declarations
  ( CaseAlternative(..)
  , DataConstructorDeclaration(..)
  , Declaration(..)
  , DeclarationRef(..)
  , ErrorMessageHint(..)
  , Expr(..)
  , GuardedExpr(..)
  , InstanceDerivationStrategy(..)
  , Module(..)
  , NameSource(..)
  , TypeDeclarationData(..)
  , TypeInstanceBody(..)
  , ValueDeclarationData(..)
  , declSourceSpan
  , isTypeClassDecl
  )
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (SourceAnn, SourceSpan, internalModuleSourceSpan, nullSourceAnn, nullSourceSpan)
import Language.PureScript.Constants.Prim as C
import Language.PureScript.Environment
  ( DataDeclType(..)
  , NameKind(..)
  , TypeClassData(..)
  , dictTypeName
  , function
  , makeTypeClassData
  , primClasses
  , primCoerceClasses
  , primIntClasses
  , primRowClasses
  , primRowListClasses
  , primSymbolClasses
  , primTypeErrorClasses
  , tyRecord
  )
import Language.PureScript.Errors
  ( MultipleErrors
  , SimpleErrorMessage(..)
  , addHint
  , errorMessage'
  , internalCompilerError
  , parU
  , rethrow
  )
import Language.PureScript.Externs (ExternsDeclaration(..), ExternsFile(..))
import Language.PureScript.Label (Label(..))
import Language.PureScript.Names
  ( ClassName
  , ConstructorName
  , Ident(..)
  , ModuleName
  , Name(..)
  , ProperName(..)
  , Qualified(..)
  , QualifiedBy(..)
  , TypeName
  , byNullSourcePos
  , coerceProperName
  , qualify
  , runIdent
  )
import Language.PureScript.PSString (PSString, mkString)
import Language.PureScript.Sugar.CaseDeclarations (desugarCases)
import Language.PureScript.TypeClassDictionaries (superclassName)
import Language.PureScript.Types
  ( Constraint(..)
  , SourceConstraint
  , SourceType
  , Type(..)
  , TypeVarVisibility(..)
  , addVisibility
  , everythingOnTypes
  , freeTypeVariables
  , moveQuantifiersToFront
  , quantify
  , replaceAllTypeVars
  , rowFromList
  , srcConstrainedType
  , srcConstraint
  , srcREmpty
  , srcRowListItem
  , srcTypeApp
  , srcTypeConstructor
  , srcTypeVar
  )

type MemberMap = Map (Tuple ModuleName (ProperName ClassName)) TypeClassData

type Desugar m = StateT MemberMap m

desugarTypeClasses
  :: forall m
   . MonadSupply m
  => MonadError MultipleErrors m
  => Array ExternsFile
  -> Module
  -> m Module
desugarTypeClasses externs m = evalStateT (desugarModule m) initialState
  where
  initialState :: MemberMap
  initialState = Map.unions
    [ mapKeysWith (qualify C.mPrim) primClasses
    , mapKeysWith (qualify C.mPrimCoerce) primCoerceClasses
    , mapKeysWith (qualify C.mPrimRow) primRowClasses
    , mapKeysWith (qualify C.mPrimRowList) primRowListClasses
    , mapKeysWith (qualify C.mPrimSymbol) primSymbolClasses
    , mapKeysWith (qualify C.mPrimInt) primIntClasses
    , mapKeysWith (qualify C.mPrimTypeError) primTypeErrorClasses
    , Map.fromFoldable (Array.concatMap (\(ExternsFile ef) ->
        mapMaybe (fromExternsDecl ef.efModuleName) ef.efDeclarations) externs)
    ]

  fromExternsDecl
    :: ModuleName
    -> ExternsDeclaration
    -> Maybe (Tuple (Tuple ModuleName (ProperName ClassName)) TypeClassData)
  fromExternsDecl mn (EDClass d) =
    Just (Tuple (Tuple mn d.edClassName)
      (makeTypeClassData d.edClassTypeArguments d.edClassMembers d.edClassConstraints d.edFunctionalDependencies d.edIsEmpty))
  fromExternsDecl _ _ = Nothing

mapKeysWith
  :: forall k v
   . Ord (Tuple ModuleName (ProperName ClassName))
  => (Qualified (ProperName k) -> Tuple ModuleName (ProperName ClassName))
  -> Map (Qualified (ProperName k)) v
  -> Map (Tuple ModuleName (ProperName ClassName)) v
mapKeysWith f m =
  Map.fromFoldable (map (\(Tuple k v) -> Tuple (f k) v) (Map.toUnfoldable m :: Array _))

desugarModule
  :: forall m
   . MonadSupply m
  => MonadError MultipleErrors m
  => Module
  -> Desugar m Module
desugarModule (Module ss coms name decls (Just exps)) = do
  let { yes: classDecls, no: restDecls } = partition isTypeClassDecl decls
      classVerts = map (\d -> Tuple d (Tuple (classDeclName d) (superClassesNames d))) classDecls
  classResults <- parU (stronglyConnComp classVerts) (desugarClassDecl name exps)
  restResults <- parU restDecls (desugarDecl name exps)
  let Tuple classNewExpss classDeclss = unzipTuples classResults
      Tuple restNewExpss restDeclss = unzipTuples restResults
  pure $ Module ss coms name
    (Array.concat restDeclss <> Array.concat classDeclss)
    (Just (exps <> mapMaybe identity restNewExpss <> mapMaybe identity classNewExpss))
  where
  desugarClassDecl
    :: ModuleName
    -> Array DeclarationRef
    -> SCC Declaration
    -> Desugar m (Tuple (Maybe DeclarationRef) (Array Declaration))
  desugarClassDecl name' exps' (AcyclicSCC d) = desugarDecl name' exps' d
  desugarClassDecl _ _ (CyclicSCC ds') =
    case NEL.fromFoldable ds' of
      Just ds'' ->
        throwError (errorMessage' (declSourceSpan (NEL.head ds''))
          (CycleInTypeClassDeclaration (map classDeclName ds'')))
      Nothing -> internalCompilerError "desugarClassDecl: empty CyclicSCC"

  superClassesNames :: Declaration -> Array (Qualified (ProperName ClassName))
  superClassesNames (TypeClassDeclaration _ _ _ implies _ _) = map constraintName implies
  superClassesNames _ = []

  constraintName :: SourceConstraint -> Qualified (ProperName ClassName)
  constraintName (Constraint c) = c.constraintClass

  classDeclName :: Declaration -> Qualified (ProperName ClassName)
  classDeclName (TypeClassDeclaration _ pn _ _ _ _) = Qualified (ByModuleName name) pn
  classDeclName _ = Qualified byNullSourcePos (ProperName "")

desugarModule _ = internalCompilerError "Exports should have been elaborated in name desugaring"

unzipTuples :: forall a b. Array (Tuple a b) -> Tuple (Array a) (Array b)
unzipTuples = Array.foldl (\(Tuple as bs) (Tuple a b) -> Tuple (Array.snoc as a) (Array.snoc bs b)) (Tuple [] [])

desugarDecl
  :: forall m
   . MonadSupply m
  => MonadError MultipleErrors m
  => ModuleName
  -> Array DeclarationRef
  -> Declaration
  -> Desugar m (Tuple (Maybe DeclarationRef) (Array Declaration))
desugarDecl mn exps = go
  where
  go d@(TypeClassDeclaration sa name args implies deps members) = do
    modify_ (Map.insert (Tuple mn name)
      (makeTypeClassData args (map memberToNameAndType members) implies deps false))
    pure (Tuple Nothing
      (Array.cons d
        (Array.cons (typeClassDictionaryDeclaration sa name args implies members)
          (map (typeClassMemberToDictionaryAccessor mn name args) members))))
  go (TypeInstanceDeclaration sa na chainId idx name deps className tys body) = do
    name' <- desugarInstName name
    let d = TypeInstanceDeclaration sa na chainId idx (Right name') deps className tys body
    let explicitOrNot = case body of
          DerivedInstance -> Left (DerivedInstancePlaceholder className KnownClassStrategy)
          NewtypeInstance -> Left (DerivedInstancePlaceholder className NewtypeStrategy)
          ExplicitInstance members -> Right members
    dictDecl <- case explicitOrNot of
      Right members
        | className == C.tyCoercible ->
            throwError (errorMessage' (fst sa) (InvalidCoercibleInstanceDeclaration tys))
        | otherwise -> do
            desugared <- desugarCases members
            typeInstanceDictionaryDeclaration sa name' mn deps className tys desugared
      Left dict ->
        let dictTy = Array.foldl srcTypeApp
              (srcTypeConstructor (map (coerceProperName <<< dictTypeName) className))
              tys
            constrainedTy = quantify (Array.foldr srcConstrainedType dictTy deps)
        in pure (ValueDeclaration (ValueDeclarationData
             { valdeclSourceAnn: sa
             , valdeclIdent: name'
             , valdeclName: Private
             , valdeclBinders: []
             , valdeclExpression: [GuardedExpr [] (TypedValue false dict constrainedTy)]
             }))
    pure (Tuple (expRef name' className tys) [d, dictDecl])
  go other = pure (Tuple Nothing [other])

  desugarInstName :: MonadSupply m => Either String Ident -> Desugar m Ident
  desugarInstName (Left s) = freshIdent s
  desugarInstName (Right i) = pure i

  expRef :: Ident -> Qualified (ProperName ClassName) -> Array SourceType -> Maybe DeclarationRef
  expRef name className tys
    | isExportedClass className && Array.all (Array.all isExportedType <<< getConstructors) tys =
        Just (TypeInstanceRef genSpan name UserNamed)
    | otherwise = Nothing

  isExportedClass :: Qualified (ProperName ClassName) -> Boolean
  isExportedClass = isExported (\pn -> Array.elem (TypeClassRef genSpan pn))

  isExportedType :: Qualified (ProperName TypeName) -> Boolean
  isExportedType = isExported (\pn refs -> isJust (Array.find (matchesTypeRef pn) refs))

  isExported
    :: forall a
     . (ProperName a -> Array DeclarationRef -> Boolean)
    -> Qualified (ProperName a)
    -> Boolean
  isExported test (Qualified (ByModuleName mn') pn) = mn /= mn' || test pn exps
  isExported _ _ = false

  matchesTypeRef :: ProperName TypeName -> DeclarationRef -> Boolean
  matchesTypeRef pn (TypeRef _ pn' _) = pn == pn'
  matchesTypeRef _ _ = false

  getConstructors :: SourceType -> Array (Qualified (ProperName TypeName))
  getConstructors = everythingOnTypes (<>) getConstructor
    where
    getConstructor (TypeConstructor _ tcname) = [tcname]
    getConstructor _ = []

  genSpan :: SourceSpan
  genSpan = internalModuleSourceSpan "<generated>"

memberToNameAndType :: Declaration -> Tuple Ident SourceType
memberToNameAndType (TypeDeclaration (TypeDeclarationData td)) = Tuple td.tydeclIdent td.tydeclType
memberToNameAndType _ = Tuple (Ident "") (srcREmpty)

typeClassDictionaryDeclaration
  :: SourceAnn
  -> ProperName ClassName
  -> Array (Tuple String (Maybe SourceType))
  -> Array SourceConstraint
  -> Array Declaration
  -> Declaration
typeClassDictionaryDeclaration sa name args implies members =
  let superclassTypes = Array.zipWith (\nm c -> Tuple nm (superclassType c))
        (superClassDictionaryNames implies) implies
      members' = map (\(Tuple i t) -> Tuple (runIdent i) t) (map memberToNameAndType members)
      mtys = members' <> superclassTypes
      toRowListItem (Tuple l t) = srcRowListItem (Label (mkString l)) t
      ctor = DataConstructorDeclaration
        { dataCtorAnn: sa
        , dataCtorName: coerceProperName (dictTypeName name)
        , dataCtorFields: [Tuple (Ident "dict")
            (srcTypeApp tyRecord (rowFromList (Tuple (map toRowListItem mtys) srcREmpty)))]
        }
  in DataDeclaration sa Newtype (coerceProperName (dictTypeName name)) args [ctor]
  where
  superclassType :: SourceConstraint -> SourceType
  superclassType (Constraint c) =
    function unitType
      (Array.foldl srcTypeApp
        (srcTypeConstructor (map (coerceProperName <<< dictTypeName) c.constraintClass))
        c.constraintArgs)

unitType :: SourceType
unitType = srcTypeApp tyRecord srcREmpty

typeClassMemberToDictionaryAccessor
  :: ModuleName
  -> ProperName ClassName
  -> Array (Tuple String (Maybe SourceType))
  -> Declaration
  -> Declaration
typeClassMemberToDictionaryAccessor mn name args (TypeDeclaration (TypeDeclarationData td)) =
  let sa = td.tydeclSourceAnn
      ss = fst sa
      ident = td.tydeclIdent
      ty = td.tydeclType
      className = Qualified (ByModuleName mn) name
      dictIdent = Ident "dict"
      dictObjIdent = Ident "v"
      ctor = ConstructorBinder ss
        (map (coerceProperName <<< dictTypeName) className)
        [VarBinder ss dictObjIdent]
      acsr = Accessor (mkString (runIdent ident))
               (Var ss (Qualified byNullSourcePos dictObjIdent))
      visibility = map (\(Tuple s _) -> Tuple s TypeVarVisible) args
      body = TypedValue false
        (Abs (VarBinder ss dictIdent)
          (Case [Var ss (Qualified byNullSourcePos dictIdent)]
            [CaseAlternative { caseAlternativeBinders: [ctor], caseAlternativeResult: [GuardedExpr [] acsr] }]))
        (addVisibility visibility
          (moveQuantifiersToFront nullSourceAnn
            (quantify (srcConstrainedType
              (srcConstraint className [] (map (srcTypeVar <<< fst) args) Nothing)
              ty))))
  in ValueDeclaration (ValueDeclarationData
       { valdeclSourceAnn: sa
       , valdeclIdent: ident
       , valdeclName: Private
       , valdeclBinders: []
       , valdeclExpression: [GuardedExpr [] body]
       })
typeClassMemberToDictionaryAccessor _ _ _ _ = ValueDeclaration (ValueDeclarationData
  { valdeclSourceAnn: nullSourceAnn
  , valdeclIdent: Ident ""
  , valdeclName: Private
  , valdeclBinders: []
  , valdeclExpression: []
  })

typeInstanceDictionaryDeclaration
  :: forall m
   . MonadError MultipleErrors m
  => SourceAnn
  -> Ident
  -> ModuleName
  -> Array SourceConstraint
  -> Qualified (ProperName ClassName)
  -> Array SourceType
  -> Array Declaration
  -> Desugar m Declaration
typeInstanceDictionaryDeclaration sa name mn deps className tys decls =
  rethrow (addHint (ErrorInInstance className tys)) do
    m <- get
    TypeClassData tcd <-
      case Map.lookup (qualify mn className) m of
        Nothing -> throwError (errorMessage' (fst sa) (UnknownName (map TyClassName className)))
        Just tc -> pure tc

    let memberTypes = map (\(Tuple (Tuple ident ty) _) ->
              Tuple ident (replaceAllTypeVars (Array.zipWith Tuple (map fst tcd.typeClassArguments) tys) ty))
              tcd.typeClassMembers

    let declaredMembers = Set.fromFoldable (mapMaybe declIdent decls)

    let unreachable = Array.any (\(Constraint c) -> c.constraintClass == C.clsFail) deps
                   && Array.null decls

    if not unreachable
      then case Array.uncons (Array.filter (\(Tuple ident _) -> not (Set.member ident declaredMembers)) memberTypes) of
        Nothing -> pure unit
        Just { head: hd, tail: tl } ->
          throwError (errorMessage' (fst sa) (MissingClassMember (NEL.cons' hd (List.fromFoldable tl))))
      else pure unit

    members <- Array.zip (map typeClassMemberName decls)
                 <$> traverse (memberToValue memberTypes) decls

    superclassesDicts <- for tcd.typeClassSuperclasses \(Constraint c) -> do
      let tyArgs = map (replaceAllTypeVars (Array.zipWith Tuple (map fst tcd.typeClassArguments) tys))
                     c.constraintArgs
      pure (Abs (VarBinder (fst sa) UnusedIdent) (DeferredDictionary c.constraintClass tyArgs))

    let superclasses = Array.zip (superClassDictionaryNames tcd.typeClassSuperclasses) superclassesDicts

    let allMembers = members <> superclasses
        props = Literal (fst sa)
                  (ObjectLiteral (map (\(Tuple l e) -> Tuple (mkString l) e) allMembers))
        dictTy = Array.foldl srcTypeApp
                   (srcTypeConstructor (map (coerceProperName <<< dictTypeName) className))
                   tys
        constrainedTy = quantify (Array.foldr srcConstrainedType dictTy deps)
        dict = App (Constructor (fst sa) (map (coerceProperName <<< dictTypeName) className)) props
        mkTV = if unreachable
               then TypedValue false (Var nullSourceSpan (Qualified (ByModuleName C.mPrim) (Ident "undefined")))
               else TypedValue true dict
        result = ValueDeclaration (ValueDeclarationData
          { valdeclSourceAnn: sa
          , valdeclIdent: name
          , valdeclName: Private
          , valdeclBinders: []
          , valdeclExpression: [GuardedExpr [] (mkTV constrainedTy)]
          })
    pure result
  where
  memberToValue :: Array (Tuple Ident SourceType) -> Declaration -> Desugar m Expr
  memberToValue tys' (ValueDeclaration (ValueDeclarationData vd))
    | [GuardedExpr [] val] <- vd.valdeclExpression = do
        case Array.find (\(Tuple i _) -> i == vd.valdeclIdent) tys' of
          Nothing -> throwError (errorMessage' (fst vd.valdeclSourceAnn)
                       (ExtraneousClassMember vd.valdeclIdent className))
          Just _ -> pure val
  memberToValue _ _ = internalCompilerError "Invalid declaration in type instance definition"

declIdent :: Declaration -> Maybe Ident
declIdent (ValueDeclaration (ValueDeclarationData vd)) = Just vd.valdeclIdent
declIdent (TypeDeclaration (TypeDeclarationData td)) = Just td.tydeclIdent
declIdent _ = Nothing

typeClassMemberName :: Declaration -> String
typeClassMemberName d = case declIdent d of
  Just i -> runIdent i
  Nothing -> ""

superClassDictionaryNames :: Array SourceConstraint -> Array String
superClassDictionaryNames supers =
  Array.zipWith (\idx (Constraint c) -> superclassName c.constraintClass idx)
    (Array.range 0 (Array.length supers - 1))
    supers
