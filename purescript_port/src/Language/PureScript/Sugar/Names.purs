module Language.PureScript.Sugar.Names
  ( desugarImports
  , externsEnv
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.State.Class (class MonadState, gets, modify_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Class (class MonadWriter)
import Data.Array as Array
import Data.Array (mapMaybe)
import Data.Foldable (foldM)
import Data.Traversable (traverse)
import Data.Map (Map)
import Data.Map as Map
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)

import Language.PureScript.AST.Binders (Binder(..), binderNamesWithSpans)
import Language.PureScript.AST.Declarations
  ( CaseAlternative(..)
  , DataConstructorDeclaration(..)
  , Declaration(..)
  , DeclarationRef(..)
  , ErrorMessageHint(..)
  , ExportSource(..)
  , Expr(..)
  , Guard(..)
  , Module(..)
  , TypeDeclarationData(..)
  , TypeFixity(..)
  , ValueDeclarationData(..)
  , ValueFixity(..)
  , declName
  , declRefName
  , declSourceSpan
  , getValueDeclaration
  , traverseDataCtorFields
  )
import Language.PureScript.AST.SourcePos (SourcePos(..), SourceSpan, spanStart)
import Language.PureScript.AST.Traversals
  ( defS
  , everywhereWithContextOnValuesM
  , sndM
  )
import Language.PureScript.Errors
  ( MultipleErrors
  , SimpleErrorMessage(..)
  , addHint
  , errorMessage
  , errorMessage''
  , internalCompilerError
  , nonEmpty
  , parU
  , warnAndRethrow
  , warnAndRethrowWithPosition
  )
import Language.PureScript.Externs
  ( ExternsDeclaration(..)
  , ExternsFile(..)
  , ExternsImport(..)
  )
import Language.PureScript.Linter.Imports (UsedImports)
import Language.PureScript.Names
  ( ClassName
  , ConstructorName
  , Ident
  , ModuleName
  , Name(..)
  , OpName
  , ProperName(..)
  , Qualified(..)
  , QualifiedBy(..)
  , TypeName
  , TypeOpName
  , ValueOpName
  , byNullSourcePos
  , disqualify
  )
import Language.PureScript.Sugar.Names.Env
  ( Env
  , Exports(..)
  , ImportProvenance(..)
  , ImportRecord(..)
  , Imports(..)
  , checkImportConflicts
  , nullImports
  , primEnv
  , nullExports
  )
import Language.PureScript.Sugar.Names.Exports (findExportable, resolveExports)
import Language.PureScript.Sugar.Names.Imports (resolveImports, resolveModuleImport)
import Language.PureScript.Types
  ( Constraint(..)
  , SourceConstraint
  , SourceType
  , Type(..)
  , everywhereOnTypesM
  )
import Data.List.NonEmpty as NEL

desugarImports
  :: forall m
   . MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => MonadState (Tuple Env UsedImports) m
  => Module
  -> m Module
desugarImports = updateEnv >=> renameInModule'
  where
  updateEnv :: Module -> m Module
  updateEnv m@(Module ss _ mn _ refs) = do
    members <- findExportable m
    env' <- gets (\(Tuple e _) -> Map.insert mn (Tuple (Tuple ss nullImports) members) e)
    Tuple m' imps <- resolveImports env' m
    exps <- case refs of
      Nothing -> pure members
      Just rs -> resolveExports env' ss mn imps members rs
    modify_ (\(Tuple e u) -> Tuple (Map.insert mn (Tuple (Tuple ss imps) exps) e) u)
    pure m'

  renameInModule' :: Module -> m Module
  renameInModule' m@(Module _ _ mn _ _) =
    warnAndRethrow (addHint (ErrorInModule mn)) do
      env <- gets fst
      case Map.lookup mn env of
        Nothing -> internalCompilerError "Module is missing in renameInModule'"
        Just (Tuple (Tuple _ imps) exps) -> do
          Tuple m' used <- runStateT (renameInModuleST imps m) Map.empty
          modify_ (\(Tuple e u) -> Tuple e (Map.unionWith (<>) u used))
          pure $ elaborateExports exps m'

externsEnv
  :: forall m
   . MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => Env
  -> ExternsFile
  -> m Env
externsEnv env (ExternsFile ef) = do
  let localSrc = ExportSource
        { exportSourceDefinedIn: ef.efModuleName
        , exportSourceImportedFrom: Nothing
        }
      efExports = ef.efExports
      efDecls   = ef.efDeclarations

      exportedTypes :: Map (ProperName TypeName) (Tuple (Array (ProperName ConstructorName)) ExportSource)
      exportedTypes = Map.fromFoldable $ mapMaybe (toExportedType localSrc efExports efDecls) efExports

      members = Exports
        { exportedTypes
        , exportedTypeOps:     exportedRefs localSrc getTypeOpRefE efExports
        , exportedTypeClasses: exportedRefs localSrc getTypeClassRefE efExports
        , exportedValues:      exportedRefs localSrc getValueRefE efExports
        , exportedValueOps:    exportedRefs localSrc getValueOpRefE efExports
        }
      env' = Map.insert ef.efModuleName (Tuple (Tuple ef.efSourceSpan nullImports) members) env
      fromEFImport (ExternsImport ei) =
        Tuple ei.eiModule
          [ { ss: ef.efSourceSpan, mTyp: Just ei.eiImportType, qual: ei.eiImportedAs } ]
  imps <- foldM (resolveModuleImport env') nullImports (map fromEFImport ef.efImports)
  exps <- resolveExports env' ef.efSourceSpan ef.efModuleName imps members efExports
  pure $ Map.insert ef.efModuleName (Tuple (Tuple ef.efSourceSpan imps) exps) env
  where
  toExportedType localSrc _ efDecls (TypeRef _ tyCon dctors) =
    let allDctors = fromMaybe (mapMaybe (forTyCon tyCon) efDecls) dctors
    in Just (Tuple tyCon (Tuple allDctors localSrc))
  toExportedType _ _ _ _ = Nothing

  forTyCon tyCon (EDDataConstructor d) | d.edDataCtorTypeCtor == tyCon = Just d.edDataCtorName
  forTyCon _ _ = Nothing

  exportedRefs :: forall a. Ord a => ExportSource -> (DeclarationRef -> Maybe a) -> Array DeclarationRef -> Map a ExportSource
  exportedRefs localSrc f refs = Map.fromFoldable (map (\a -> Tuple a localSrc) (mapMaybe f refs))

  getTypeOpRefE (TypeOpRef _ op) = Just op
  getTypeOpRefE _ = Nothing

  getTypeClassRefE (TypeClassRef _ n) = Just n
  getTypeClassRefE _ = Nothing

  getValueRefE (ValueRef _ n) = Just n
  getValueRefE _ = Nothing

  getValueOpRefE (ValueOpRef _ op) = Just op
  getValueOpRefE _ = Nothing

elaborateExports :: Exports -> Module -> Module
elaborateExports exps (Module ss coms mn decls refs) =
  let Exports expsRec = exps
      elaboratedTypeRefs :: Array DeclarationRef
      elaboratedTypeRefs = map (\(Tuple tctor (Tuple dctors src)) ->
        let ExportSource srcRec = src
            ref = TypeRef ss tctor (Just dctors)
        in if mn == srcRec.exportSourceDefinedIn then ref else ReExportRef ss src ref)
        (Map.toUnfoldable expsRec.exportedTypes :: Array _)

      goRef :: forall a. (a -> DeclarationRef) -> (Exports -> Map a ExportSource) -> Array DeclarationRef
      goRef toRef select = map (\(Tuple export src) ->
        let ExportSource srcRec = src
        in if mn == srcRec.exportSourceDefinedIn then toRef export else ReExportRef ss src (toRef export))
        (Map.toUnfoldable (select exps) :: Array _)

      allRefs = elaboratedTypeRefs
             <> goRef (TypeOpRef ss) (\(Exports e) -> e.exportedTypeOps)
             <> goRef (TypeClassRef ss) (\(Exports e) -> e.exportedTypeClasses)
             <> goRef (ValueRef ss) (\(Exports e) -> e.exportedValues)
             <> goRef (ValueOpRef ss) (\(Exports e) -> e.exportedValueOps)
  in Module ss coms mn decls (Just (reorderExports decls refs allRefs))

reorderExports :: Array Declaration -> Maybe (Array DeclarationRef) -> Array DeclarationRef -> Array DeclarationRef
reorderExports decls originalRefs refs =
  let names = fromMaybe (mapMaybe declName decls) (map (map declRefName) originalRefs)
      namesMap = Map.fromFoldable (Array.zipWith Tuple names (Array.range 0 (Array.length names - 1)))
      originalIndex ref = Map.lookup (declRefName ref) namesMap
  in Array.sortBy (\a b -> compare (originalIndex a) (originalIndex b)) refs

-- | `renameInModuleST` runs inside StateT UsedImports, which can be nested inside the outer monad.
renameInModuleST
  :: forall m
   . MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => Imports
  -> Module
  -> StateT UsedImports m Module
renameInModuleST imports m = do
  let decls = case m of Module _ _ _ ds _ -> ds
      exps  = case m of Module _ _ _ _ e  -> e
      Module modSS coms mn _ _ = m
  decls' <- parU decls (renameDecl imports modSS mn)
  pure (Module modSS coms mn decls' exps)

renameDecl
  :: forall m
   . MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => Imports
  -> SourceSpan
  -> ModuleName
  -> Declaration
  -> StateT UsedImports m Declaration
renameDecl imports _modSS mn decl = lift do
  let
    Imports imps = imports
    s0 = Tuple (declSourceSpan decl) (Map.empty :: Map Ident SourcePos)

    -- Resolve a name via the import map to its defining module.
    update
      :: forall a. Ord a
      => Map (Qualified a) (Array (ImportRecord a))
      -> (a -> Name)
      -> Qualified a
      -> SourceSpan
      -> m (Qualified a)
    update impMap toName qname@(Qualified qb name) ss =
      warnAndRethrowWithPosition ss $
        case Tuple (Map.lookup qname impMap) qb of
          Tuple (Just options) _ -> do
            Tuple _ srcMn <- checkImportConflicts ss mn toName options
            pure (Qualified (ByModuleName srcMn) name)
          Tuple Nothing (ByModuleName mn'') ->
            if Set.member mn'' imps.importedQualModules || Set.member mn'' imps.importedModules
            then throwError (errorMessage (UnknownName (map toName qname)))
            else throwError (errorMessage (UnknownName (Qualified byNullSourcePos (ModName mn''))))
          _ ->
            throwError (errorMessage (UnknownName (map toName qname)))

    updateTypeName :: Qualified (ProperName TypeName) -> SourceSpan -> m (Qualified (ProperName TypeName))
    updateTypeName = update imps.importedTypes TyName

    updateTypeOpName :: Qualified (OpName TypeOpName) -> SourceSpan -> m (Qualified (OpName TypeOpName))
    updateTypeOpName = update imps.importedTypeOps TyOpName

    updateDataConstructorName :: Qualified (ProperName ConstructorName) -> SourceSpan -> m (Qualified (ProperName ConstructorName))
    updateDataConstructorName = update imps.importedDataConstructors DctorName

    updateClassName :: Qualified (ProperName ClassName) -> SourceSpan -> m (Qualified (ProperName ClassName))
    updateClassName = update imps.importedTypeClasses TyClassName

    updateValueName :: Qualified Ident -> SourceSpan -> m (Qualified Ident)
    updateValueName = update imps.importedValues IdentName

    updateValueOpName :: Qualified (OpName ValueOpName) -> SourceSpan -> m (Qualified (OpName ValueOpName))
    updateValueOpName = update imps.importedValueOps ValOpName

    -- Resolve type-level names. Called by everywhereOnTypesM (bottom-up),
    -- so constraint args and sub-types are already resolved when we see them.
    resolveTypeRef :: SourceType -> m SourceType
    resolveTypeRef (TypeConstructor ann name) =
      TypeConstructor ann <$> updateTypeName name (fst ann)
    resolveTypeRef (TypeOp ann name) =
      TypeOp ann <$> updateTypeOpName name (fst ann)
    resolveTypeRef (ConstrainedType ann (Constraint c) ty) = do
      cls' <- updateClassName c.constraintClass (fst ann)
      pure (ConstrainedType ann (Constraint c { constraintClass = cls' }) ty)
    resolveTypeRef t = pure t

    resolveType :: SourceType -> m SourceType
    resolveType = everywhereOnTypesM resolveTypeRef

    -- Resolve all names in a constraint (for declarations, where everywhereOnTypesM won't run).
    resolveConstraint :: SourceConstraint -> m SourceConstraint
    resolveConstraint (Constraint c) = do
      cls' <- updateClassName c.constraintClass (fst c.constraintAnn)
      ks'  <- traverse resolveType c.constraintKindArgs
      ts'  <- traverse resolveType c.constraintArgs
      pure (Constraint c { constraintClass = cls', constraintKindArgs = ks', constraintArgs = ts' })

    -- Resolve kind annotations on type parameters.
    resolveTypeArgs :: Array (Tuple String (Maybe SourceType)) -> m (Array (Tuple String (Maybe SourceType)))
    resolveTypeArgs = traverse (sndM (traverse resolveType))

    updateDecl' :: Tuple SourceSpan (Map Ident SourcePos) -> Declaration -> m (Tuple (Tuple SourceSpan (Map Ident SourcePos)) Declaration)
    updateDecl' (Tuple _ bound) d = do
      let ss = declSourceSpan d
      d' <- case d of
        DataDeclaration sa dtype name args dctors -> do
          args'   <- resolveTypeArgs args
          dctors' <- traverse (traverseDataCtorFields (traverse (sndM resolveType))) dctors
          pure (DataDeclaration sa dtype name args' dctors')
        TypeSynonymDeclaration sa name args ty -> do
          args' <- resolveTypeArgs args
          ty'   <- resolveType ty
          pure (TypeSynonymDeclaration sa name args' ty')
        KindDeclaration sa kindFor name ty -> do
          ty' <- resolveType ty
          pure (KindDeclaration sa kindFor name ty')
        TypeDeclaration (TypeDeclarationData td) -> do
          ty' <- resolveType td.tydeclType
          pure (TypeDeclaration (TypeDeclarationData td { tydeclType = ty' }))
        ExternDeclaration sa ident ty -> do
          ty' <- resolveType ty
          pure (ExternDeclaration sa ident ty')
        ExternDataDeclaration sa name ki -> do
          ki' <- resolveType ki
          pure (ExternDataDeclaration sa name ki')
        TypeClassDeclaration sa name args implies deps ds -> do
          args'    <- resolveTypeArgs args
          implies' <- traverse resolveConstraint implies
          pure (TypeClassDeclaration sa name args' implies' deps ds)
        TypeInstanceDeclaration sa na ch idx name cs className args body -> do
          cs'        <- traverse resolveConstraint cs
          className' <- updateClassName className ss
          args'      <- traverse resolveType args
          pure (TypeInstanceDeclaration sa na ch idx name cs' className' args' body)
        FixityDeclaration sa (Left (ValueFixity fix alias op)) -> do
          alias' <- case alias of
            Qualified qb (Left ident) ->
              Qualified qb <<< Left <<< disqualify <$> updateValueName (Qualified qb ident) ss
            Qualified qb (Right ctor) ->
              Qualified qb <<< Right <<< disqualify <$> updateDataConstructorName (Qualified qb ctor) ss
          pure (FixityDeclaration sa (Left (ValueFixity fix alias' op)))
        FixityDeclaration sa (Right (TypeFixity fix alias op)) -> do
          alias' <- updateTypeName alias ss
          pure (FixityDeclaration sa (Right (TypeFixity fix alias' op)))
        _ -> pure d
      pure (Tuple (Tuple ss bound) d')

    updateValue' :: Tuple SourceSpan (Map Ident SourcePos) -> Expr -> m (Tuple (Tuple SourceSpan (Map Ident SourcePos)) Expr)
    updateValue' state@(Tuple pos bound) expr = case expr of
      PositionedValue pos' _ _ ->
        pure (Tuple (Tuple pos' bound) expr)
      Abs (VarBinder ss arg) _ ->
        pure (Tuple (Tuple pos (Map.insert arg (spanStart ss) bound)) expr)
      Var ss qname@(Qualified qb ident) ->
        case qb of
          BySourcePos (SourcePos sp) | sp.line == 0 && sp.column == 0 ->
            case Map.lookup ident bound of
              Just sourcePos ->
                pure (Tuple (Tuple ss bound) (Var ss (Qualified (BySourcePos sourcePos) ident)))
              Nothing ->
                (\q -> Tuple (Tuple ss bound) (Var ss q)) <$> updateValueName qname ss
          ByModuleName _ ->
            (\q -> Tuple (Tuple ss bound) (Var ss q)) <$> updateValueName qname ss
          _ ->
            -- non-null BySourcePos: already locally qualified
            pure (Tuple (Tuple ss bound) expr)
      Op ss op ->
        (\o -> Tuple (Tuple ss bound) (Op ss o)) <$> updateValueOpName op ss
      Constructor ss name ->
        (\n -> Tuple (Tuple ss bound) (Constructor ss n)) <$> updateDataConstructorName name ss
      TypedValue check val ty -> do
        ty' <- resolveType ty
        pure (Tuple state (TypedValue check val ty'))
      Let w ds _ ->
        let letBound = Array.foldl (\acc d -> case d of
              ValueDeclaration (ValueDeclarationData vd) ->
                Map.insert vd.valdeclIdent (spanStart (fst vd.valdeclSourceAnn)) acc
              _ -> acc) bound ds
        in pure (Tuple (Tuple pos letBound) expr)
      _ -> pure (Tuple state expr)

    updateBinder' :: Tuple SourceSpan (Map Ident SourcePos) -> Binder -> m (Tuple (Tuple SourceSpan (Map Ident SourcePos)) Binder)
    updateBinder' state@(Tuple _ bound) binder = case binder of
      PositionedBinder pos _ _ ->
        pure (Tuple (Tuple pos bound) binder)
      ConstructorBinder ss name bs ->
        (\n -> Tuple (Tuple ss bound) (ConstructorBinder ss n bs)) <$> updateDataConstructorName name ss
      OpBinder ss op ->
        (\o -> Tuple (Tuple ss bound) (OpBinder ss o)) <$> updateValueOpName op ss
      TypedBinder ty b -> do
        ty' <- resolveType ty
        pure (Tuple state (TypedBinder ty' b))
      _ -> pure (Tuple state binder)

    updateCaseAlt' :: Tuple SourceSpan (Map Ident SourcePos) -> CaseAlternative -> m (Tuple (Tuple SourceSpan (Map Ident SourcePos)) CaseAlternative)
    updateCaseAlt' (Tuple pos bound) ca@(CaseAlternative cas) =
      let newBindings = cas.caseAlternativeBinders >>= binderNamesWithSpans
          newBound = Array.foldl (\acc (Tuple ss i) -> Map.insert i (spanStart ss) acc) bound newBindings
      in pure (Tuple (Tuple pos newBound) ca)

    traversal = everywhereWithContextOnValuesM s0 updateDecl' updateValue' updateBinder' updateCaseAlt' defS defS

  traversal.decl decl

when :: forall m. Applicative m => Boolean -> m Unit -> m Unit
when true  m = m
when false _ = pure unit
