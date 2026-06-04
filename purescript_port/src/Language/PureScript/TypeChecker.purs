-- | Top-level type checker module.
module Language.PureScript.TypeChecker
  ( module Language.PureScript.TypeChecker.Kinds
  , module Language.PureScript.TypeChecker.Monad
  , module Language.PureScript.TypeChecker.Roles
  , module Language.PureScript.TypeChecker.Synonyms
  , module Language.PureScript.TypeChecker.Unify
  , typeCheckModule
  , checkNewtype
  ) where

import Prelude

import Control.Monad (when, unless)
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.State (gets)
import Control.Monad.State.Class (class MonadState, modify_)
import Control.Monad.Supply.Class (class MonadSupply)
import Control.Monad.Writer.Class (class MonadWriter, tell)
import Data.Array as Array
import Data.String as String
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse, for)
import Data.Tuple (Tuple(..), fst, snd)

import Language.PureScript.AST.Declarations
  ( DataConstructorDeclaration(..)
  , Declaration(..)
  , DeclarationRef(..)
  , ErrorMessageHint(..)
  , ExportSource
  , GuardedExpr(..)
  , ImportDeclarationType
  , KindSignatureFor(..)
  , Module(..)
  , RoleDeclarationData(..)
  , TypeDeclarationData(..)
  , TypeInstanceBody(..)
  , ValueDeclarationData(..)
  , declRefSourceSpan
  , declSourceSpan
  , traverseTypeInstanceBody
  )
import Language.PureScript.AST.SourcePos (SourceAnn, SourceSpan)
import Language.PureScript.AST.Declarations.ChainId (ChainId)
import Language.PureScript.Environment
  ( DataDeclType(..)
  , Environment(..)
  , FunctionalDependency
  , NameKind(..)
  , NameVisibility(..)
  , TypeClassData(..)
  , TypeKind(..)
  , makeTypeClassData
  , tyFunction
  )
import Language.PureScript.Errors
  ( MultipleErrors
  , SimpleErrorMessage(..)
  , addHint
  , errorMessage
  , errorMessage'
  , internalCompilerError
  , positionedError
  , rethrow
  , warnAndRethrow
  )
import Language.PureScript.Names
  ( ClassName
  , ConstructorName
  , Ident(..)
  , ModuleName(..)
  , ProperName(..)
  , Qualified(..)
  , QualifiedBy(..)
  , TypeName
  , coerceProperName
  , disqualify
  , isPlainIdent
  , mkQualified
  , runProperName
  )
import Language.PureScript.Roles (Role(..))
import Language.PureScript.Sugar.Names.Env (Exports(..))
import Language.PureScript.TypeChecker.Kinds
  ( checkInstanceDeclaration
  , checkKindDeclaration
  , checkTypeKind
  , kindOfClass
  , kindOfData
  , kindOfTypeSynonym
  , kindOfWithUnknowns
  , kindsOfAll
  , kindType
  )
import Language.PureScript.TypeChecker.Kinds as Kinds
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.TypeChecker.Monad
  ( CheckState(..)
  , getEnv
  , getTypeClassDictionariesForModule
  , guardWith
  , lookupVariable
  , putEnv
  )
import Language.PureScript.TypeChecker.Roles
  ( checkRoleDeclarationArity
  , checkRoles
  , inferDataBindingGroupRoles
  , inferRoles
  )
import Language.PureScript.TypeChecker.Synonyms
import Language.PureScript.TypeChecker.Types (BindingGroupType(..), typesOf)
import Language.PureScript.TypeChecker.Unify
import Language.PureScript.TypeChecker.Unify (varIfUnknown)
import Language.PureScript.TypeClassDictionaries (NamedDict, TypeClassDictionaryInScope(..))
import Language.PureScript.TypeChecker.Kinds (unapplyTypes)
import Language.PureScript.Types
  ( Constraint(..)
  , SourceConstraint
  , SourceType
  , Type(..)
  , everythingOnTypes
  , overConstraintArgs
  , srcInstanceType
  )

-- ---------------------------------------------------------------------------
-- Helpers not yet in the port
-- ---------------------------------------------------------------------------

-- | Check if a type contains a ForAll.
containsForAll :: forall a. Type a -> Boolean
containsForAll = everythingOnTypes (||) go
  where
  go (ForAll _ _ _ _ _ _) = true
  go _ = false

-- | Compute the arity of a kind (number of function arguments).
kindArity :: SourceType -> Int
kindArity k = Array.length (fst (unapplyKinds k))
  where
  unapplyKinds :: SourceType -> Tuple (Array SourceType) SourceType
  unapplyKinds = go []
    where
    go ks (TypeApp _ (TypeApp _ fn k1) k2)
      | fn == tyFunction = go (Array.snoc ks k1) k2
    go ks (ForAll _ _ _ _ k _) = go ks k
    go ks k = Tuple ks k

-- | Generate nominal roles for a kind.
nominalRolesForKind :: SourceType -> Array Role
nominalRolesForKind k = Array.replicate (kindArity k) Nominal

-- | Check if a name is a dictionary type name ($Dict suffix).
isDictTypeName :: forall a. ProperName a -> Boolean
isDictTypeName (ProperName s) =
  let suffix = "$Dict"
      sLen = String.length suffix
      strLen = String.length s
  in strLen >= sLen && String.drop (strLen - sLen) s == suffix

-- ---------------------------------------------------------------------------
-- Export check utilities
-- ---------------------------------------------------------------------------

-- | Check if a type synonym has no unexpanded type synonyms.
checkTypeSynonyms
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => SourceType
  -> m Unit
checkTypeSynonyms ty = void (replaceAllTypeSynonyms ty)

-- ---------------------------------------------------------------------------
-- checkNewtype
-- ---------------------------------------------------------------------------

-- | Check that a newtype declaration has exactly one constructor with one field.
checkNewtype
  :: forall m
   . MonadError MultipleErrors m
  => ProperName TypeName
  -> Array DataConstructorDeclaration
  -> m (Tuple DataConstructorDeclaration (Tuple Ident SourceType))
checkNewtype name decls = case decls of
  [decl@(DataConstructorDeclaration d)] -> case d.dataCtorFields of
    [field] -> pure (Tuple decl field)
    _ -> throwError (errorMessage (InvalidNewtype name))
  _ -> throwError (errorMessage (InvalidNewtype name))

-- ---------------------------------------------------------------------------
-- typeCheckModule
-- ---------------------------------------------------------------------------

-- | Type check an entire module.
typeCheckModule
  :: forall m
   . MonadSupply m
  => MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => Map ModuleName Exports
  -> Module
  -> m Module
typeCheckModule _ (Module _ _ _ _ Nothing) =
  internalCompilerError "exports should have been elaborated before typeCheckModule"
typeCheckModule modulesExports (Module ss coms mn decls (Just exps)) =
  warnAndRethrow (addHint (ErrorInModule mn)) $ do
    let importOrDecl = map fromImportDecl decls
        decls' = Array.mapMaybe leftMaybe importOrDecl
        imports = Array.mapMaybe rightMaybe importOrDecl
    modify_ \(CheckState s) -> CheckState s
      { checkCurrentModule = Just mn
      , checkCurrentModuleImports = imports
      }
    decls'' <- typeCheckAll mn decls'
    checkSuperClassesAreExported <- getSuperClassExportCheck
    for_ exps \e -> do
      checkTypesAreExported e
      checkClassMembersAreExported e
      checkClassesAreExported e
      checkSuperClassesAreExported e
      checkDataConstructorsAreExported e
    pure (Module ss coms mn (map toImportDecl imports <> decls'') (Just exps))
  where

  leftMaybe :: forall a b. Either a b -> Maybe a
  leftMaybe (Left a) = Just a
  leftMaybe _ = Nothing

  rightMaybe :: forall a b. Either a b -> Maybe b
  rightMaybe (Right b) = Just b
  rightMaybe _ = Nothing

  fromImportDecl
    :: Declaration
    -> Either Declaration
        (Tuple SourceAnn
          (Tuple ModuleName
            (Tuple ImportDeclarationType
              (Tuple (Maybe ModuleName)
                (Map (ProperName TypeName) (Tuple (Array (ProperName ConstructorName)) ExportSource))))))
  fromImportDecl (ImportDeclaration sa importedModuleName importDeclarationType asModuleName) =
    Right (Tuple sa (Tuple importedModuleName (Tuple importDeclarationType
      (Tuple asModuleName
        (case Map.lookup importedModuleName modulesExports of
          Nothing -> Map.empty
          Just (Exports exs) -> exs.exportedTypes)))))
  fromImportDecl d = Left d

  toImportDecl
    :: Tuple SourceAnn
        (Tuple ModuleName
          (Tuple ImportDeclarationType
            (Tuple (Maybe ModuleName)
              (Map (ProperName TypeName) (Tuple (Array (ProperName ConstructorName)) ExportSource)))))
    -> Declaration
  toImportDecl (Tuple sa (Tuple importedModuleName (Tuple importDeclarationType (Tuple asModuleName _)))) =
    ImportDeclaration sa importedModuleName importDeclarationType asModuleName

  qualify' :: forall a. a -> Qualified a
  qualify' = Qualified (ByModuleName mn)

  getSuperClassExportCheck = do
    classesToSuperClasses <- gets \(CheckState s) ->
      let Environment e = s.checkEnv
      in map (\(TypeClassData tc) ->
          Set.fromFoldable
            (Array.mapMaybe
              (\(Constraint c) ->
                let Qualified qb _ = c.constraintClass
                in case qb of
                  ByModuleName mn' | mn' == mn -> Just c.constraintClass
                  _ -> Nothing)
              tc.typeClassSuperclasses))
        e.typeClasses
    let transitiveSuperClassesFor qname =
          untilSame
            (\s -> s <> Array.foldl (\acc n -> acc <> fromMaybe Set.empty (Map.lookup n classesToSuperClasses)) Set.empty (Array.fromFoldable s))
            (fromMaybe Set.empty (Map.lookup qname classesToSuperClasses))
        superClassesFor qname =
          fromMaybe Set.empty (Map.lookup qname classesToSuperClasses)
    pure (checkSuperClassExport superClassesFor transitiveSuperClassesFor)

  moduleClassExports :: Set (Qualified (ProperName ClassName))
  moduleClassExports = Set.fromFoldable (Array.mapMaybe (\ref -> case ref of
    TypeClassRef _ name -> Just (qualify' name)
    _ -> Nothing) exps)

  untilSame :: forall a. Eq a => (a -> a) -> a -> a
  untilSame f a =
    let a' = f a
    in if a == a' then a else untilSame f a'

  checkMemberExport :: (SourceType -> Array DeclarationRef) -> DeclarationRef -> m Unit
  checkMemberExport extract dr@(TypeRef _ name dctors) = do
    env <- getEnv
    let Environment e = env
    for_ (Map.lookup (qualify' name) e.types) \(Tuple k _) ->
      checkExport dr (extract k)
    for_ (Map.lookup (qualify' name) e.typeSynonyms) \(Tuple _ ty) ->
      checkExport dr (extract ty)
    for_ dctors \dctors' ->
      for_ dctors' \dctor ->
        for_ (Map.lookup (qualify' dctor) e.dataConstructors) \(Tuple (Tuple (Tuple _ _) ty) _) ->
          checkExport dr (extract ty)
  checkMemberExport extract dr@(ValueRef _ name) = do
    ty <- lookupVariable (qualify' name)
    checkExport dr (extract ty)
  checkMemberExport _ _ = pure unit

  checkSuperClassExport
    :: (Qualified (ProperName ClassName) -> Set (Qualified (ProperName ClassName)))
    -> (Qualified (ProperName ClassName) -> Set (Qualified (ProperName ClassName)))
    -> DeclarationRef
    -> m Unit
  checkSuperClassExport superClassesFor transitiveSuperClassesFor dr@(TypeClassRef drss className) = do
    let superClasses = superClassesFor (qualify' className)
        transitiveSuperClasses = transitiveSuperClassesFor (qualify' className)
        unexported = Set.difference superClasses moduleClassExports
    unless (Set.isEmpty unexported)
      (throwError (errorMessage' drss
        (TransitiveExportError dr
          (map (\n -> TypeClassRef drss (disqualify n))
            (Array.fromFoldable transitiveSuperClasses)))))
  checkSuperClassExport _ _ _ = pure unit

  checkExport :: DeclarationRef -> Array DeclarationRef -> m Unit
  checkExport dr drs = case Array.filter (not <<< exported) drs of
    [] -> pure unit
    hidden -> throwError (errorMessage' (declRefSourceSpan dr) (TransitiveExportError dr (Array.nubByEq nubEq hidden)))
    where
    exported e = Array.any (exports e) exps
    exports (TypeRef _ pn1 _) (TypeRef _ pn2 _) = pn1 == pn2
    exports (ValueRef _ id1) (ValueRef _ id2) = id1 == id2
    exports (TypeClassRef _ pn1) (TypeClassRef _ pn2) = pn1 == pn2
    exports _ _ = false
    nubEq (TypeRef _ pn1 _) (TypeRef _ pn2 _) = pn1 == pn2
    nubEq r1 r2 = r1 == r2

  checkTypesAreExported :: DeclarationRef -> m Unit
  checkTypesAreExported ref = checkMemberExport findTcons ref
    where
    findTcons :: SourceType -> Array DeclarationRef
    findTcons = everythingOnTypes (<>) go
      where
      go (TypeConstructor _ (Qualified (ByModuleName mn') name)) | mn' == mn =
        [TypeRef (declRefSourceSpan ref) name Nothing]
      go _ = []

  checkClassesAreExported :: DeclarationRef -> m Unit
  checkClassesAreExported ref = checkMemberExport findClasses ref
    where
    findClasses :: SourceType -> Array DeclarationRef
    findClasses = everythingOnTypes (<>) go
      where
      go (ConstrainedType _ (Constraint c) _) =
        Array.mapMaybe (Just <<< TypeClassRef (declRefSourceSpan ref))
          (extractCurrentModuleClass c.constraintClass)
      go _ = []
    extractCurrentModuleClass :: Qualified (ProperName ClassName) -> Array (ProperName ClassName)
    extractCurrentModuleClass (Qualified (ByModuleName mn') name) | mn == mn' = [name]
    extractCurrentModuleClass _ = []

  checkClassMembersAreExported :: DeclarationRef -> m Unit
  checkClassMembersAreExported dr@(TypeClassRef ss' name) = do
    let members = map (ValueRef ss') (fromMaybe [] (Array.findMap findClassMembers decls))
        missingMembers = Array.filter (\m -> not (Array.elem m exps)) members
    unless (Array.null missingMembers)
      (throwError (errorMessage' ss' (TransitiveExportError dr missingMembers)))
    where
    findClassMembers :: Declaration -> Maybe (Array Ident)
    findClassMembers (TypeClassDeclaration _ name' _ _ _ ds) | name == name' =
      Just (map extractMemberName ds)
    findClassMembers (DataBindingGroupDeclaration ds) =
      Array.findMap findClassMembers (Array.fromFoldable (NEL.toList ds))
    findClassMembers _ = Nothing
    extractMemberName :: Declaration -> Ident
    extractMemberName (TypeDeclaration (TypeDeclarationData td)) = td.tydeclIdent
    extractMemberName _ = Ident "<<invalid class member>>"
  checkClassMembersAreExported _ = pure unit

  checkDataConstructorsAreExported :: DeclarationRef -> m Unit
  checkDataConstructorsAreExported dr@(TypeRef ss' name exportedDctorsMaybe) =
    if Array.null (fromMaybe [] exportedDctorsMaybe)
    then
      -- No constructors exported — warn if Generic/Newtype instance exists
      for_ [clsGeneric, clsNewtype] \className -> do
        env <- getEnv
        let Environment e = env
            dicts = case Map.lookup (ByModuleName mn) e.typeClassDictionaries of
              Nothing -> []
              Just m -> case Map.lookup className m of
                Nothing -> []
                Just m2 -> Array.concatMap (\(Tuple _ arr) -> arr) (Map.toUnfoldable m2)
            isDictOfTypeRef (TypeClassDictionaryInScope d) =
              case Array.index d.tcdInstanceTypes 0 of
                Just firstTy ->
                  let Tuple ctor _ = unapplyTypes firstTy
                  in case ctor of
                    TypeConstructor _ (Qualified (ByModuleName mn') n) | mn' == mn && n == name -> true
                    _ -> false
                Nothing -> false
        when (Array.any isDictOfTypeRef dicts)
          (tell (errorMessage' ss' (HiddenConstructors dr className)))
    else do
      env <- getEnv
      let Environment e = env
          dataCtorNames = fromMaybe [] do
            Tuple _ tk <- Map.lookup (mkQualified name mn) e.types
            getDataCtorNames tk
          exportedDctors = fromMaybe [] exportedDctorsMaybe
          missingDctors = Array.filter (\d -> not (Array.elem d exportedDctors)) dataCtorNames
      unless (Array.null missingDctors)
        (throwError (errorMessage' ss' (TransitiveDctorExportError dr missingDctors)))
    where
    getDataCtorNames :: TypeKind -> Maybe (Array (ProperName ConstructorName))
    getDataCtorNames (DataType _ _ constructors) = Just (map fst constructors)
    getDataCtorNames _ = Nothing
  checkDataConstructorsAreExported _ = pure unit

  -- Known class names for Generic/Newtype orphan checks
  clsGeneric :: Qualified (ProperName ClassName)
  clsGeneric = Qualified (ByModuleName (ModuleName "Data.Generic.Rep")) (ProperName "Generic")

  clsNewtype :: Qualified (ProperName ClassName)
  clsNewtype = Qualified (ByModuleName (ModuleName "Data.Newtype")) (ProperName "Newtype")

-- ---------------------------------------------------------------------------
-- typeCheckAll
-- ---------------------------------------------------------------------------

typeCheckAll
  :: forall m
   . MonadSupply m
  => MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => ModuleName
  -> Array Declaration
  -> m (Array Declaration)
typeCheckAll moduleName = traverse go
  where
  go :: Declaration -> m Declaration
  go (DataDeclaration sa@(Tuple ss _) dtype name args dctors) =
    warnAndRethrow (addHint (ErrorInTypeConstructor name) <<< addHint (positionedError ss)) do
      when (dtype == Newtype) (void (checkNewtype name dctors))
      checkDuplicateTypeArguments (map fst args)
      Tuple dataCtors ctorKind <- kindOfData moduleName (Tuple sa (Tuple name (Tuple args dctors)))
      let args' = args `withKinds` ctorKind
      env <- getEnv
      dctors' <- traverse (replaceTypeSynonymsInDataConstructor <<< fst) dataCtors
      let args'' = args' `withRoles` inferRoles env moduleName name args' dctors'
      addDataType moduleName dtype name args'' dataCtors ctorKind
      pure (DataDeclaration sa dtype name args dctors)

  go d@(DataBindingGroupDeclaration tys) = do
    let tysList = Array.fromFoldable (NEL.toList tys)
        syns = Array.mapMaybe toTypeSynonym tysList
        dataDecls = Array.mapMaybe toDataDecl tysList
        roleDecls = Array.mapMaybe toRoleDecl tysList
        clss = Array.mapMaybe toClassDecl tysList
        bindingGroupNames = Array.nub
          (map (\(Tuple _ (Tuple n _)) -> n) (Array.mapMaybe toTypeSynonymName tysList)
          <> map (\(Tuple _ (Tuple _ (Tuple n _))) -> n) (Array.mapMaybe toDataDeclName tysList)
          <> map (\n -> coerceProperName n) (Array.mapMaybe toClassDeclName tysList))
        sss = map declSourceSpan tys
    warnAndRethrow
      (addHint (ErrorInDataBindingGroup bindingGroupNames) <<<
        addHint (PositionedError sss)) do
      env <- getEnv
      Tuple syn_ks (Tuple data_ks cls_ks) <- kindsOfAll moduleName syns (map snd dataDecls) (map snd clss)
      for_ (Array.zip syns syn_ks) \(Tuple (Tuple _ (Tuple name (Tuple args _))) (Tuple elabTy kind)) -> do
        checkDuplicateTypeArguments (map fst args)
        let args' = args `withKinds` kind
        addTypeSynonym moduleName name args' elabTy kind
      let dataDeclsWithKinds = Array.zipWith
            (\(Tuple dtype (Tuple _ (Tuple name (Tuple args _)))) (Tuple dataCtors ctorKind) ->
              Tuple dtype (Tuple name (Tuple (args `withKinds` ctorKind) (Tuple dataCtors ctorKind))))
            dataDecls data_ks
      inferRoles' <- do
        groups <- for dataDeclsWithKinds \(Tuple _ (Tuple name (Tuple args (Tuple dataCtors _)))) -> do
          dctors' <- traverse (replaceTypeSynonymsInDataConstructor <<< fst) dataCtors
          pure (Tuple name (Tuple args dctors'))
        pure (inferDataBindingGroupRoles env moduleName roleDecls groups)
      for_ dataDeclsWithKinds \(Tuple dtype (Tuple name (Tuple args' (Tuple dataCtors ctorKind)))) -> do
        when (dtype == Newtype) (void (checkNewtype name (map fst dataCtors)))
        checkDuplicateTypeArguments (map fst args')
        let args'' = args' `withRoles` inferRoles' (mkQualified name moduleName) args'
        addDataType moduleName dtype name args'' dataCtors ctorKind
      for_ roleDecls (checkRoleDeclaration moduleName)
      for_ (Array.zip clss cls_ks) \(Tuple (Tuple deps (Tuple sa (Tuple pn _))) (Tuple args' (Tuple implies' (Tuple tys' kind)))) -> do
        let qualifiedClassName = Qualified (ByModuleName moduleName) pn
        guardWith (errorMessage (DuplicateTypeClass pn (fst sa)))
          (not (Map.member qualifiedClassName (let Environment e = env in e.typeClasses)))
        addTypeClass moduleName qualifiedClassName (map (\(Tuple v k) -> Tuple v (Just k)) args') implies' deps tys' kind
      pure d
    where
    toTypeSynonym (TypeSynonymDeclaration sa nm args ty) = Just (Tuple sa (Tuple nm (Tuple args ty)))
    toTypeSynonym _ = Nothing
    toTypeSynonymName (TypeSynonymDeclaration _ nm _ _) = Just (Tuple unit (Tuple nm unit))
    toTypeSynonymName _ = Nothing
    toDataDecl (DataDeclaration sa dtype nm args dctors) = Just (Tuple dtype (Tuple sa (Tuple nm (Tuple args dctors))))
    toDataDecl _ = Nothing
    toDataDeclName (DataDeclaration _ _ nm _ _) = Just (Tuple unit (Tuple unit (Tuple nm unit)))
    toDataDeclName _ = Nothing
    toRoleDecl (RoleDeclaration rdd) = Just rdd
    toRoleDecl _ = Nothing
    toClassDecl (TypeClassDeclaration sa nm args implies deps ds) =
      Just (Tuple deps (Tuple sa (Tuple nm (Tuple args (Tuple implies ds)))))
    toClassDecl _ = Nothing
    toClassDeclName (TypeClassDeclaration _ nm _ _ _ _) = Just nm
    toClassDeclName _ = Nothing

  go (TypeSynonymDeclaration sa@(Tuple ss _) name args ty) =
    warnAndRethrow (addHint (ErrorInTypeSynonym name) <<< addHint (positionedError ss)) do
      checkDuplicateTypeArguments (map fst args)
      Tuple elabTy kind <- kindOfTypeSynonym moduleName (Tuple sa (Tuple name (Tuple args ty)))
      let args' = args `withKinds` kind
      addTypeSynonym moduleName name args' elabTy kind
      pure (TypeSynonymDeclaration sa name args ty)

  go (KindDeclaration sa@(Tuple ss _) kindFor name ty) =
    warnAndRethrow (addHint (ErrorInKindDeclaration name) <<< addHint (positionedError ss)) do
      elabTy <- withFreshSubstitution (checkKindDeclaration moduleName ty)
      env <- getEnv
      let Environment e = env
      putEnv (Environment e { types = Map.insert (Qualified (ByModuleName moduleName) name) (Tuple elabTy LocalTypeVariable) e.types })
      pure (KindDeclaration sa kindFor name elabTy)

  go d@(RoleDeclaration rdd) = do
    checkRoleDeclaration moduleName rdd
    pure d

  go (TypeDeclaration _) =
    internalCompilerError "Type declarations should have been removed before typeCheckAll"

  go (ValueDeclaration (ValueDeclarationData vd)) =
    warnAndRethrow
      ((if isPlainIdent vd.valdeclIdent then addHint (ErrorInValueDeclaration vd.valdeclIdent) else identity)
        <<< addHint (positionedError (fst vd.valdeclSourceAnn))) do
      case Tuple vd.valdeclBinders vd.valdeclExpression of
        Tuple [] [GuardedExpr [] val] -> do
          valueIsNotDefined moduleName vd.valdeclIdent
          result <- typesOf NonRecursiveBindingGroup moduleName
            [Tuple (Tuple vd.valdeclSourceAnn vd.valdeclIdent) val]
          case result of
            [Tuple _ (Tuple val'' ty)] -> do
              addValue moduleName vd.valdeclIdent ty vd.valdeclName
              pure (ValueDeclaration (ValueDeclarationData vd { valdeclExpression = [GuardedExpr [] val''] }))
            _ -> internalCompilerError "typesOf did not return a singleton"
        _ -> internalCompilerError "Binders were not desugared"

  go (BoundValueDeclaration _ _ _) =
    internalCompilerError "BoundValueDeclaration should be desugared"

  go (BindingGroupDeclaration vals) = do
    let sss = map (\(Tuple (Tuple sa _) _) -> fst sa) vals
    warnAndRethrow
      (addHint (ErrorInBindingGroup (map (\(Tuple (Tuple _ ident) _) -> ident) vals))
        <<< addHint (PositionedError sss)) do
      for_ (NEL.toList vals) \(Tuple (Tuple _ ident) _) ->
        valueIsNotDefined moduleName ident
      let valsList = Array.fromFoldable (NEL.toList vals)
      tys <- typesOf RecursiveBindingGroup moduleName
        (map (\(Tuple sai (Tuple _ expr)) -> Tuple sai expr) valsList)
      vals'' <- for (Array.mapMaybe (\(Tuple sai@(Tuple _ name) (Tuple nameKind _)) ->
          case Array.findMap (\(Tuple (Tuple _ name') (Tuple val ty)) ->
            if name == name' then Just (Tuple val ty) else Nothing) tys of
            Just (Tuple val ty) -> Just (Tuple sai (Tuple nameKind (Tuple val ty)))
            Nothing -> Nothing
          ) valsList) \(Tuple sai@(Tuple _ name) (Tuple nameKind (Tuple val ty))) -> do
        addValue moduleName name ty nameKind
        pure (Tuple sai (Tuple nameKind val))
      case NEL.fromFoldable vals'' of
        Nothing -> internalCompilerError "BindingGroupDeclaration: empty vals''"
        Just nonEmpty -> pure (BindingGroupDeclaration nonEmpty)

  go d@(ExternDataDeclaration (Tuple ss _) name kind) =
    warnAndRethrow (addHint (ErrorInForeignImportData name) <<< addHint (positionedError ss)) do
      elabKind <- withFreshSubstitution (checkKindDeclaration moduleName kind)
      env <- getEnv
      let Environment e = env
          qualName = Qualified (ByModuleName moduleName) name
          roles = nominalRolesForKind elabKind
      putEnv (Environment e { types = Map.insert qualName (Tuple elabKind (ExternData roles)) e.types })
      pure d

  go d@(ExternDeclaration (Tuple ss _) name ty) =
    warnAndRethrow (addHint (ErrorInForeignImport name) <<< addHint (positionedError ss)) do
      env <- getEnv
      Tuple elabTy _ <- withFreshSubstitution do
        Tuple ty' unks <- kindOfWithUnknowns ty
        ty'' <- varIfUnknown unks ty'
        pure (Tuple ty'' unit)
      let Environment e = env
      case Map.lookup (Qualified (ByModuleName moduleName) name) e.names of
        Just _ -> throwError (errorMessage (RedefinedIdent name))
        Nothing -> putEnv (Environment e
          { names = Map.insert
              (Qualified (ByModuleName moduleName) name)
              (Tuple (Tuple elabTy External) Defined)
              e.names })
      pure d

  go d@(FixityDeclaration _ _) = pure d
  go d@(ImportDeclaration _ _ _ _) = pure d

  go d@(TypeClassDeclaration sa@(Tuple ss _) pn args implies deps tys) =
    warnAndRethrow (addHint (ErrorInTypeClassDeclaration pn) <<< addHint (positionedError ss)) do
      env <- getEnv
      let Environment e = env
          qualifiedClassName = Qualified (ByModuleName moduleName) pn
      guardWith (errorMessage (DuplicateTypeClass pn ss))
        (not (Map.member qualifiedClassName e.typeClasses))
      Tuple args' (Tuple implies' (Tuple tys' kind)) <-
        kindOfClass moduleName (Tuple sa (Tuple pn (Tuple args (Tuple implies tys))))
      addTypeClass moduleName qualifiedClassName (map (\(Tuple v k) -> Tuple v (Just k)) args') implies' deps tys' kind
      pure d

  go (TypeInstanceDeclaration _ _ _ _ (Left _) _ _ _ _) =
    internalCompilerError "typeCheckAll: type class instance generated name should have been desugared"

  go d@(TypeInstanceDeclaration sa@(Tuple ss _) _ ch idx (Right dictName) deps className tys body) =
    rethrow (addHint (ErrorInInstance className tys) <<< addHint (positionedError ss)) do
      env <- getEnv
      let Environment e = env
          qualifiedDictName = Qualified (ByModuleName moduleName) dictName
      -- Check for duplicate instance
      for_ (Map.values e.typeClassDictionaries) \classDicts ->
        for_ (Map.values classDicts) \identDicts ->
          guardWith (errorMessage (DuplicateInstance dictName ss))
            (not (Map.member qualifiedDictName identDicts))
      case Map.lookup className e.typeClasses of
        Nothing -> internalCompilerError "typeCheckAll: Encountered unknown type class in instance declaration"
        Just typeClass@(TypeClassData tc) -> do
          checkInstanceArity dictName className typeClass tys
          Tuple deps' (Tuple kinds' (Tuple tys' vars)) <- withFreshSubstitution
            (checkInstanceDeclaration moduleName
              (Tuple sa (Tuple deps (Tuple className tys))))
          tys'' <- traverse replaceAllTypeSynonyms tys'
          for_ (Array.zip (Array.range 0 (Array.length tys'' - 1)) tys'')
            \(Tuple i t) -> checkTypeClassInstance typeClass i t
          let nonOrphanModules = findNonOrphanModules className typeClass tys''
          checkOrphanInstance dictName className tys'' nonOrphanModules
          let chainId = Just ch
          checkOverlappingInstance ss chainId dictName vars className typeClass tys'' nonOrphanModules
          _ <- traverseTypeInstanceBody checkInstanceMembers body
          deps'' <- traverse (\c -> overConstraintArgs (traverse replaceAllTypeSynonyms) c) deps'
          let dict = TypeClassDictionaryInScope
                { tcdChain: chainId
                , tcdIndex: idx
                , tcdValue: qualifiedDictName
                , tcdPath: []
                , tcdClassName: className
                , tcdForAll: vars
                , tcdInstanceKinds: kinds'
                , tcdInstanceTypes: tys''
                , tcdDependencies: Just deps''
                , tcdDescription:
                    if isPlainIdent dictName
                    then Nothing
                    else Just (srcInstanceType ss vars className tys'')
                }
          addTypeClassDictionaries (ByModuleName moduleName)
            (Map.singleton className (Map.singleton (Qualified (ByModuleName moduleName) dictName) [dict]))
          pure d

  checkInstanceArity :: Ident -> Qualified (ProperName ClassName) -> TypeClassData -> Array SourceType -> m Unit
  checkInstanceArity dictName className (TypeClassData tc) tys = do
    let typeClassArity = Array.length tc.typeClassArguments
        instanceArity = Array.length tys
    when (typeClassArity /= instanceArity)
      (throwError (errorMessage (ClassInstanceArityMismatch dictName className typeClassArity instanceArity)))

  checkInstanceMembers :: Array Declaration -> m (Array Declaration)
  checkInstanceMembers instDecls = do
    let idents = Array.sort (map memberName instDecls)
        dups = firstDuplicate idents
    for_ dups \ident ->
      throwError (errorMessage (DuplicateValueDeclaration ident))
    pure instDecls
    where
    memberName :: Declaration -> Ident
    memberName (ValueDeclaration (ValueDeclarationData vd)) = vd.valdeclIdent
    memberName _ = Ident "<<invalid member>>"
    firstDuplicate :: Array Ident -> Maybe Ident
    firstDuplicate arr = case Array.uncons arr of
      Just { head: x, tail } -> case Array.uncons tail of
        Just { head: y } | x == y -> Just x
        Just _ -> firstDuplicate tail
        Nothing -> Nothing
      Nothing -> Nothing

  findNonOrphanModules
    :: Qualified (ProperName ClassName)
    -> TypeClassData
    -> Array SourceType
    -> Set ModuleName
  findNonOrphanModules (Qualified (ByModuleName mn') _) (TypeClassData tc) tys' =
    Set.insert mn' nonOrphanModules'
    where
    typeModule :: SourceType -> Maybe ModuleName
    typeModule (TypeVar _ _) = Nothing
    typeModule (TypeLevelString _ _) = Nothing
    typeModule (TypeLevelInt _ _) = Nothing
    typeModule (TypeConstructor _ (Qualified (ByModuleName mn'') _)) = Just mn''
    typeModule (TypeConstructor _ (Qualified (BySourcePos _) _)) = Nothing -- unqualified
    typeModule (TypeApp _ t1 _) = typeModule t1
    typeModule (KindApp _ t1 _) = typeModule t1
    typeModule (KindedType _ t1 _) = typeModule t1
    typeModule _ = Nothing

    modulesByTypeIndex :: Map Int (Maybe ModuleName)
    modulesByTypeIndex = Map.fromFoldable (Array.zip (Array.range 0 (Array.length tys' - 1)) (map typeModule tys'))

    lookupModule :: Int -> Set ModuleName
    lookupModule idx' = case Map.lookup idx' modulesByTypeIndex of
      Just (Just m) -> Set.singleton m
      Just Nothing -> Set.empty
      Nothing -> Set.empty -- fallback

    nonOrphanModules' :: Set ModuleName
    nonOrphanModules' =
      let coveringSets = Array.fromFoldable tc.typeClassCoveringSets
          computeSet covSet =
            Array.foldl (\acc i -> Set.union acc (lookupModule i)) Set.empty (Array.fromFoldable covSet)
      in case Array.uncons coveringSets of
         Nothing -> Set.empty
         Just { head, tail } ->
           Array.foldl Set.intersection (computeSet head) (map computeSet tail)

  findNonOrphanModules _ _ _ = Set.empty

  checkOverlappingInstance
    :: SourceSpan
    -> Maybe ChainId
    -> Ident
    -> Array (Tuple String SourceType)
    -> Qualified (ProperName ClassName)
    -> TypeClassData
    -> Array SourceType
    -> Set ModuleName
    -> m Unit
  checkOverlappingInstance ss ch dictName vars className (TypeClassData tc) tys' nonOrphanModules =
    for_ (Array.fromFoldable nonOrphanModules) \m -> do
      dicts <- getTypeClassDictionariesForModule (ByModuleName m)
      let classDicts = case Map.lookup className dicts of
            Nothing -> []
            Just m2 -> Array.concatMap (\(Tuple qualIdent arr) ->
              map (Tuple qualIdent) arr) (Map.toUnfoldable m2)
      for_ classDicts \(Tuple (Qualified mn' ident) dict) ->
        let TypeClassDictionaryInScope d = dict
        in if ch == d.tcdChain ||
              instancesAreApart tc.typeClassCoveringSets tys' d.tcdInstanceTypes
           then pure unit
           else do
             let this = if isPlainIdent dictName
                        then Right dictName
                        else Left (srcInstanceType ss vars className tys')
                 that = case d.tcdDescription of
                   Just desc -> Qualified mn' (Left desc)
                   Nothing   -> Qualified mn' (Right ident)
             throwError (errorMessage
               (OverlappingInstances className tys'
                 [that, Qualified (ByModuleName moduleName) this]))

  instancesAreApart
    :: Set (Set Int)
    -> Array SourceType
    -> Array SourceType
    -> Boolean
  instancesAreApart sets lhs rhs =
    Array.all (\covSet -> Array.any typesApart (Array.fromFoldable covSet)) (Array.fromFoldable sets)
    where
    typesApart :: Int -> Boolean
    typesApart i =
      let l = Array.index lhs i
          r = Array.index rhs i
      in case Tuple l r of
        Tuple (Just lt) (Just rt) -> typeHeadsApart lt rt
        _ -> false

    typeHeadsApart :: SourceType -> SourceType -> Boolean
    typeHeadsApart l r | l == r = false
    typeHeadsApart (TypeVar _ _) _ = false
    typeHeadsApart _ (TypeVar _ _) = false
    typeHeadsApart (KindedType _ t1 _) t2 = typeHeadsApart t1 t2
    typeHeadsApart t1 (KindedType _ t2 _) = typeHeadsApart t1 t2
    typeHeadsApart (TypeApp _ h1 t1) (TypeApp _ h2 t2) =
      typeHeadsApart h1 h2 || typeHeadsApart t1 t2
    typeHeadsApart _ _ = true

  checkOrphanInstance
    :: Ident
    -> Qualified (ProperName ClassName)
    -> Array SourceType
    -> Set ModuleName
    -> m Unit
  checkOrphanInstance dictName className tys' nonOrphanModules
    | Set.member moduleName nonOrphanModules = pure unit
    | otherwise = throwError (errorMessage
        (OrphanInstance dictName className (Array.fromFoldable nonOrphanModules) tys'))

  checkTypeClassInstance
    :: TypeClassData
    -> Int
    -> SourceType
    -> m Unit
  checkTypeClassInstance (TypeClassData tc) i = check
    where
    isFunDepDetermined = Set.member i tc.typeClassDeterminedArguments
    check :: SourceType -> m Unit
    check = case _ of
      TypeVar _ _ -> pure unit
      TypeLevelString _ _ -> pure unit
      TypeLevelInt _ _ -> pure unit
      TypeConstructor _ _ -> pure unit
      TypeApp _ t1 t2 -> check t1 *> check t2
      KindApp _ t k -> check t *> check k
      KindedType _ t _ -> check t
      REmpty _ | isFunDepDetermined -> pure unit
      RCons _ _ hd tl | isFunDepDetermined -> check hd *> check tl
      ty -> throwError (errorMessage (InvalidInstanceHead ty))

  -- | withKinds: zip type args with their inferred kinds from a ForAll chain
  withKinds :: Array (Tuple String (Maybe SourceType)) -> SourceType -> Array (Tuple String (Maybe SourceType))
  withKinds args kind = case Array.uncons args of
    Nothing -> []
    Just { head: Tuple s mbk, tail: rest } -> case kind of
      ForAll _ _ _ _ k _ -> withKinds args k
      TypeApp _ (TypeApp _ fn k1) k2 | fn == tyFunction ->
        case mbk of
          Just _ -> Array.cons (Tuple s mbk) (withKinds rest k2)
          Nothing -> Array.cons (Tuple s (Just k1)) (withKinds rest k2)
      _ -> args -- fallback: return args unchanged if kind doesn't match expected shape

  withRoles :: Array (Tuple String (Maybe SourceType)) -> Array Role -> Array (Tuple String (Tuple (Maybe SourceType) Role))
  withRoles = Array.zipWith \(Tuple v k) r -> Tuple v (Tuple k r)

  replaceTypeSynonymsInDataConstructor :: DataConstructorDeclaration -> m DataConstructorDeclaration
  replaceTypeSynonymsInDataConstructor (DataConstructorDeclaration dc) = do
    fields' <- traverse (\(Tuple i ty) -> Tuple i <$> replaceAllTypeSynonyms ty) dc.dataCtorFields
    pure (DataConstructorDeclaration dc { dataCtorFields = fields' })

-- ---------------------------------------------------------------------------
-- Helper functions (local to typeCheckModule but defined at module level)
-- ---------------------------------------------------------------------------

addDataType
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => ModuleName
  -> DataDeclType
  -> ProperName TypeName
  -> Array (Tuple String (Tuple (Maybe SourceType) Role))
  -> Array (Tuple DataConstructorDeclaration SourceType)
  -> SourceType
  -> m Unit
addDataType moduleName dtype name args dctors ctorKind = do
  env <- getEnv
  let Environment e = env
      qualName = Qualified (ByModuleName moduleName) name
      hasSig = Map.member qualName e.types
      mapDataCtor (DataConstructorDeclaration dc) = Tuple dc.dataCtorName (map snd dc.dataCtorFields)
  putEnv (Environment e
    { types = Map.insert qualName
        (Tuple ctorKind (DataType dtype (map toArg args) (map (mapDataCtor <<< fst) dctors)))
        e.types })
  unless (hasSig || isDictTypeNamePS name || not (containsForAll ctorKind))
    (tell (errorMessage (MissingKindDeclaration (if dtype == Newtype then NewtypeSig else DataSig) name ctorKind)))
  for_ dctors \(Tuple (DataConstructorDeclaration dc) polyType) ->
    warnAndRethrow (addHint (ErrorInDataConstructor dc.dataCtorName))
      (addDataConstructor moduleName dtype name dc.dataCtorName dc.dataCtorFields polyType)
  where
  toArg :: Tuple String (Tuple (Maybe SourceType) Role) -> Tuple (Tuple String (Maybe SourceType)) Role
  toArg (Tuple v (Tuple k r)) = Tuple (Tuple v k) r

isDictTypeNamePS :: forall a. ProperName a -> Boolean
isDictTypeNamePS (ProperName s) =
  let suffix = "$Dict"
      sLen = String.length suffix
      strLen = String.length s
  in strLen >= sLen && String.drop (strLen - sLen) s == suffix

addDataConstructor
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => ModuleName
  -> DataDeclType
  -> ProperName TypeName
  -> ProperName ConstructorName
  -> Array (Tuple Ident SourceType)
  -> SourceType
  -> m Unit
addDataConstructor moduleName dtype name dctor dctorArgs polyType = do
  env <- getEnv
  let Environment e = env
      fields = map fst dctorArgs
  checkTypeSynonyms polyType
  putEnv (Environment e
    { dataConstructors = Map.insert
        (Qualified (ByModuleName moduleName) dctor)
        (Tuple (Tuple (Tuple dtype name) polyType) fields)
        e.dataConstructors })

checkRoleDeclaration
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => ModuleName
  -> RoleDeclarationData
  -> m Unit
checkRoleDeclaration moduleName (RoleDeclarationData rd) =
  warnAndRethrow
    (addHint (ErrorInRoleDeclaration rd.rdeclIdent) <<< addHint (positionedError (fst rd.rdeclSourceAnn)))
    do
      env <- getEnv
      let Environment e = env
          qualName = Qualified (ByModuleName moduleName) rd.rdeclIdent
      case Map.lookup qualName e.types of
        Just (Tuple kind (DataType dtype args dctors)) -> do
          checkRoleDeclarationArity rd.rdeclIdent rd.rdeclRoles (Array.length args)
          checkRoles (map (\(Tuple (Tuple v k) r) -> Tuple v (Tuple k r)) args) rd.rdeclRoles
          let args' = Array.zipWith (\(Tuple (Tuple v k) _) r -> Tuple (Tuple v k) r) args rd.rdeclRoles
          putEnv (Environment e { types = Map.insert qualName (Tuple kind (DataType dtype args' dctors)) e.types })
        Just (Tuple kind (ExternData _)) -> do
          checkRoleDeclarationArity rd.rdeclIdent rd.rdeclRoles (kindArity kind)
          putEnv (Environment e { types = Map.insert qualName (Tuple kind (ExternData rd.rdeclRoles)) e.types })
        _ -> internalCompilerError "Unsupported role declaration"

addTypeSynonym
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => ModuleName
  -> ProperName TypeName
  -> Array (Tuple String (Maybe SourceType))
  -> SourceType
  -> SourceType
  -> m Unit
addTypeSynonym moduleName name args ty kind = do
  env <- getEnv
  let Environment e = env
      qualName = Qualified (ByModuleName moduleName) name
      hasSig = Map.member qualName e.types
  checkTypeSynonyms ty
  unless (hasSig || not (containsForAll kind))
    (tell (errorMessage (MissingKindDeclaration TypeSynonymSig name kind)))
  putEnv (Environment e
    { types = Map.insert qualName (Tuple kind TypeSynonym) e.types
    , typeSynonyms = Map.insert qualName (Tuple args ty) e.typeSynonyms })

addTypeClass
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => ModuleName
  -> Qualified (ProperName ClassName)
  -> Array (Tuple String (Maybe SourceType))
  -> Array SourceConstraint
  -> Array FunctionalDependency
  -> Array Declaration
  -> SourceType
  -> m Unit
addTypeClass _ qualifiedClassName args implies dependencies ds kind = do
  env <- getEnv
  let Environment e = env
      qualName = map coerceProperName qualifiedClassName
      hasSig = Map.member qualName e.types
  newClass <- mkNewClass env
  unless (hasSig || not (containsForAll kind))
    (tell (errorMessage (MissingKindDeclaration ClassSig (disqualify qualName) kind)))
  putEnv (Environment e
    { types = Map.insert qualName (Tuple kind (ExternData (nominalRolesForKind kind))) e.types
    , typeClasses = Map.insert qualifiedClassName newClass e.typeClasses })
  where
  classMembers :: Array (Tuple Ident SourceType)
  classMembers = Array.mapMaybe toPair ds

  mkNewClass :: Environment -> m TypeClassData
  mkNewClass env = do
    implies' <- traverse (\c -> overConstraintArgs (traverse replaceAllTypeSynonyms) c) implies
    let ctIsEmpty = Array.null classMembers &&
          Array.all (\c ->
            let TypeClassData tc = findSuperClass env c
            in tc.typeClassIsEmpty) implies'
    pure (makeTypeClassData args classMembers implies' dependencies ctIsEmpty)
    where
    findSuperClass :: Environment -> SourceConstraint -> TypeClassData
    findSuperClass (Environment e) (Constraint c) =
      fromMaybe (makeTypeClassData [] [] [] [] false)
        (Map.lookup c.constraintClass e.typeClasses)

  toPair :: Declaration -> Maybe (Tuple Ident SourceType)
  toPair (TypeDeclaration (TypeDeclarationData td)) = Just (Tuple td.tydeclIdent td.tydeclType)
  toPair _ = Nothing

addTypeClassDictionaries
  :: forall m
   . MonadState CheckState m
  => QualifiedBy
  -> Map (Qualified (ProperName ClassName)) (Map (Qualified Ident) (Array NamedDict))
  -> m Unit
addTypeClassDictionaries qb entries =
  modify_ \(CheckState s) ->
    let Environment e = s.checkEnv
    in CheckState s
      { checkEnv = Environment e
          { typeClassDictionaries =
              Map.insertWith (Map.unionWith (Map.unionWith (<>))) qb entries e.typeClassDictionaries
          }
      }

valueIsNotDefined
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => ModuleName
  -> Ident
  -> m Unit
valueIsNotDefined moduleName name = do
  env <- getEnv
  let Environment e = env
  case Map.lookup (Qualified (ByModuleName moduleName) name) e.names of
    Just _ -> throwError (errorMessage (RedefinedIdent name))
    Nothing -> pure unit

addValue
  :: forall m
   . MonadState CheckState m
  => ModuleName
  -> Ident
  -> SourceType
  -> NameKind
  -> m Unit
addValue moduleName name ty nameKind = do
  env <- getEnv
  let Environment e = env
  putEnv (Environment e
    { names = Map.insert
        (Qualified (ByModuleName moduleName) name)
        (Tuple (Tuple ty nameKind) Defined)
        e.names })

checkDuplicateTypeArguments
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => Array String
  -> m Unit
checkDuplicateTypeArguments args =
  case firstDup of
    Nothing -> pure unit
    Just dup -> throwError (errorMessage (DuplicateTypeArgument dup))
  where
  firstDup :: Maybe String
  firstDup = Array.findMap (\x ->
    if Array.length (Array.filter (_ == x) args) > 1 then Just x else Nothing) args
