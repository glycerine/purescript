module Language.PureScript.Sugar.Names.Exports
  ( findExportable
  , resolveExports
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Writer.Class (class MonadWriter)
import Data.Array as Array
import Data.Foldable (foldM, traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Unsafe.Coerce (unsafeCoerce)

import Data.Either (Either(..))
import Language.PureScript.AST.Declarations
  ( DataConstructorDeclaration(..)
  , Declaration(..)
  , DeclarationRef(..)
  , ErrorMessageHint(..)
  , ExportSource(..)
  , Module(..)
  , TypeDeclarationData(..)
  , TypeFixity(..)
  , ValueDeclarationData(..)
  , ValueFixity(..)
  , declRefSourceSpan
  , declSourceSpan
  , getTypeClassRef
  , getTypeOpRef
  , getTypeRef
  , getValueOpRef
  , getValueRef
  )
import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.Errors
  ( MultipleErrors
  , SimpleErrorMessage(..)
  , addHint
  , errorMessage'
  , internalCompilerError
  , rethrow
  , rethrowWithPosition
  , warnAndRethrow
  )
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
  , disqualifyFor
  , isQualifiedWith
  , isUnqualified
  )
import Language.PureScript.Sugar.Names.Common (warnDuplicateRefs)
import Language.PureScript.Sugar.Names.Env
  ( Env
  , ExportMode(..)
  , Exports(..)
  , ImportRecord(..)
  , Imports(..)
  , checkImportConflicts
  , envModuleExports
  , exportType
  , exportTypeClass
  , exportTypeOp
  , exportValue
  , exportValueOp
  , nullExports
  )

findExportable :: forall m. MonadError MultipleErrors m => Module -> m Exports
findExportable (Module _ _ mn ds _) =
  rethrow (addHint (ErrorInModule mn)) $ foldM updateExports' nullExports ds
  where
  updateExports' :: Exports -> Declaration -> m Exports
  updateExports' exps decl = rethrowWithPosition (declSourceSpan decl) $ updateExports exps decl

  source :: ExportSource
  source = ExportSource
    { exportSourceDefinedIn: mn
    , exportSourceImportedFrom: Nothing
    }

  updateExports :: Exports -> Declaration -> m Exports
  updateExports exps (TypeClassDeclaration (Tuple ss _) tcn _ _ _ ds') = do
    exps' <- rethrowWithPosition ss $ exportTypeClass ss Internal exps tcn source
    foldM (go exps') exps' ds'
    where
    go _ acc (TypeDeclaration (TypeDeclarationData td)) =
      let Tuple ss' _ = td.tydeclSourceAnn
      in exportValue ss' acc td.tydeclIdent source
    go _ acc _ = pure acc
  updateExports exps (DataDeclaration (Tuple ss _) _ tn _ dcs) =
    exportType ss Internal exps tn (map (\(DataConstructorDeclaration d) -> d.dataCtorName) dcs) source
  updateExports exps (TypeSynonymDeclaration (Tuple ss _) tn _ _) =
    exportType ss Internal exps tn [] source
  updateExports exps (ExternDataDeclaration (Tuple ss _) tn _) =
    exportType ss Internal exps tn [] source
  updateExports exps (ValueDeclaration (ValueDeclarationData vd)) =
    exportValue (fst vd.valdeclSourceAnn) exps vd.valdeclIdent source
  updateExports exps (FixityDeclaration (Tuple ss _) (Left (ValueFixity _ _ op))) =
    exportValueOp ss exps op source
  updateExports exps (FixityDeclaration (Tuple ss _) (Right (TypeFixity _ _ op))) =
    exportTypeOp ss exps op source
  updateExports exps (ExternDeclaration (Tuple ss _) name _) =
    exportValue ss exps name source
  updateExports exps _ = pure exps

resolveExports
  :: forall m
   . MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => Env
  -> SourceSpan
  -> ModuleName
  -> Imports
  -> Exports
  -> Array DeclarationRef
  -> m Exports
resolveExports env ss mn imps exps refs =
  warnAndRethrow (addHint (ErrorInModule mn)) do
    filtered <- filterModule mn exps refs
    exps' <- foldM elaborateModuleExports filtered refs
    warnDuplicateRefs ss DuplicateExportRef refs
    pure exps'

  where

  elaborateModuleExports :: Exports -> DeclarationRef -> m Exports
  elaborateModuleExports (Exports result) (ModuleRef _ name) | name == mn = do
    let Exports expsRec = exps
    pure $ Exports result
      { exportedTypes       = Map.unionWith (\a _ -> a) result.exportedTypes expsRec.exportedTypes
      , exportedTypeOps     = Map.union result.exportedTypeOps expsRec.exportedTypeOps
      , exportedTypeClasses = Map.union result.exportedTypeClasses expsRec.exportedTypeClasses
      , exportedValues      = Map.union result.exportedValues expsRec.exportedValues
      , exportedValueOps    = Map.union result.exportedValueOps expsRec.exportedValueOps
      }
  elaborateModuleExports result (ModuleRef ss' name) = do
    let Imports imp = imps
    let isPseudo = isPseudoModule name
    when (not isPseudo && not (isImportedModule name))
      (throwError (errorMessage' ss' (UnknownExport (ModName name))))
    reTypes     <- extract ss' isPseudo name TyName     imp.importedTypes
    reTypeOps   <- extract ss' isPseudo name TyOpName   imp.importedTypeOps
    reDctors    <- extract ss' isPseudo name DctorName  imp.importedDataConstructors
    reClasses   <- extract ss' isPseudo name TyClassName imp.importedTypeClasses
    reValues    <- extract ss' isPseudo name IdentName  imp.importedValues
    reValueOps  <- extract ss' isPseudo name ValOpName  imp.importedValueOps
    let typeExports = resolveTypeExports reTypes reDctors
    m1 <- foldM (\e (Tuple (Tuple tctor dctors) src) -> exportType ss' ReExport e tctor dctors src)
           result typeExports
    m2 <- foldM (\e (Tuple op src) -> exportTypeOp ss' e op src)
           m1 (map resolveTypeOp reTypeOps)
    m3 <- foldM (\e (Tuple cls src) -> exportTypeClass ss' ReExport e cls src)
           m2 (map resolveClass reClasses)
    m4 <- foldM (\e (Tuple v src) -> exportValue ss' e v src)
           m3 (map resolveValue reValues)
    foldM (\e (Tuple op src) -> exportValueOp ss' e op src)
           m4 (map resolveValueOp reValueOps)
  elaborateModuleExports result _ = pure result

  extract
    :: forall a
     . Ord a
    => SourceSpan
    -> Boolean
    -> ModuleName
    -> (a -> Name)
    -> Map (Qualified a) (Array (ImportRecord a))
    -> m (Array (Qualified a))
  extract ss' useQual name toName importMap = do
    let pairs = Map.toUnfoldable importMap :: Array _
    Array.foldM (\acc (Tuple name' options) -> do
      let isMatch = if useQual
                    then isQualifiedWith name name'
                    else Array.any (\(ImportRecord ir) -> isUnqualified name' && isQualifiedWith name ir.importName) options
      if isMatch
        then do
          when (Array.length options > 1) $
            void (checkImportConflicts ss' mn toName options)
          case Array.head options of
            Just (ImportRecord r) -> pure (Array.snoc acc r.importName)
            Nothing -> pure acc
        else pure acc) [] pairs

  isPseudoModule :: ModuleName -> Boolean
  isPseudoModule mn' =
    let Imports imp = imps
    in testQuals imp.importedTypes mn'
    || testQuals imp.importedTypeOps mn'
    || testQuals imp.importedDataConstructors mn'
    || testQuals imp.importedTypeClasses mn'
    || testQuals imp.importedValues mn'
    || testQuals imp.importedValueOps mn'
    || testQuals imp.importedKinds mn'
    where
    testQuals :: forall a b. Map (Qualified a) b -> ModuleName -> Boolean
    testQuals m mn'' = Array.any (isQualifiedWith mn'') (Array.fromFoldable (Map.keys m))

  isImportedModule :: ModuleName -> Boolean
  isImportedModule name =
    let Imports imp = imps
    in Array.elem name (Array.fromFoldable imp.importedModules)

  resolveTypeExports
    :: Array (Qualified (ProperName TypeName))
    -> Array (Qualified (ProperName ConstructorName))
    -> Array (Tuple (Tuple (ProperName TypeName) (Array (ProperName ConstructorName))) ExportSource)
  resolveTypeExports tctors dctors = mapMaybe go tctors
    where
    go (Qualified (ByModuleName mn'') name) = do
      exps' <- envModuleExports <$> Map.lookup mn'' env
      let Exports expsRec' = exps'
      Tuple dctors' src <- Map.lookup name expsRec'.exportedTypes
      let relevantDctors = mapMaybe (disqualifyFor (Just mn'')) dctors
          ExportSource srcRec = src
      pure (Tuple (Tuple name (Array.intersect relevantDctors dctors'))
                  (ExportSource srcRec { exportSourceImportedFrom = Just mn'' }))
    go _ = Nothing

  resolveTypeOp :: Qualified (OpName TypeOpName) -> Tuple (OpName TypeOpName) ExportSource
  resolveTypeOp op = fromMaybe (coerceQualified op dummySrc) (resolve (\(Exports e) -> e.exportedTypeOps) op)

  resolveClass :: Qualified (ProperName ClassName) -> Tuple (ProperName ClassName) ExportSource
  resolveClass cls = fromMaybe (Tuple (ProperName "") dummySrc) (resolve (\(Exports e) -> e.exportedTypeClasses) cls)

  resolveValue :: Qualified Ident -> Tuple Ident ExportSource
  resolveValue ident = fromMaybe (coerceQualified ident dummySrc) (resolve (\(Exports e) -> e.exportedValues) ident)

  resolveValueOp :: Qualified (OpName ValueOpName) -> Tuple (OpName ValueOpName) ExportSource
  resolveValueOp op = fromMaybe (coerceQualified op dummySrc) (resolve (\(Exports e) -> e.exportedValueOps) op)

  dummySrc :: ExportSource
  dummySrc = ExportSource { exportSourceImportedFrom: Nothing, exportSourceDefinedIn: mn }

  coerceQualified :: forall a b. Qualified a -> b -> Tuple a b
  coerceQualified (Qualified _ a) b = Tuple a b

  resolve
    :: forall a
     . Ord a
    => (Exports -> Map a ExportSource)
    -> Qualified a
    -> Maybe (Tuple a ExportSource)
  resolve f (Qualified (ByModuleName mn'') a) = do
    exps' <- envModuleExports <$> Map.lookup mn'' env
    src <- Map.lookup a (f exps')
    let ExportSource srcRec = src
    pure (Tuple a (ExportSource srcRec { exportSourceImportedFrom = Just mn'' }))
  resolve _ _ = Nothing

filterModule
  :: forall m
   . MonadError MultipleErrors m
  => ModuleName
  -> Exports
  -> Array DeclarationRef
  -> m Exports
filterModule mn exps refs = do
  types    <- foldM filterTypes Map.empty (combineTypeRefs refs)
  typeOps  <- foldM (filterExport TyOpName getTypeOpRef (\(Exports e) -> e.exportedTypeOps)) Map.empty refs
  classes  <- foldM (filterExport TyClassName getTypeClassRef (\(Exports e) -> e.exportedTypeClasses)) Map.empty refs
  values   <- foldM (filterExport IdentName getValueRef (\(Exports e) -> e.exportedValues)) Map.empty refs
  valueOps <- foldM (filterExport ValOpName getValueOpRef (\(Exports e) -> e.exportedValueOps)) Map.empty refs
  pure $ Exports
    { exportedTypes:       types
    , exportedTypeOps:     typeOps
    , exportedTypeClasses: classes
    , exportedValues:      values
    , exportedValueOps:    valueOps
    }
  where

  combineTypeRefs :: Array DeclarationRef -> Array DeclarationRef
  combineTypeRefs =
    map (\(Tuple ss' (Tuple tc dcs)) -> TypeRef ss' tc dcs)
    <<< mapMaybe (foldl1Maybe combineEntry)
    <<< groupByTypeName
    <<< Array.sortBy (\(Tuple _ (Tuple tc1 _)) (Tuple _ (Tuple tc2 _)) -> compare tc1 tc2)
    <<< mapMaybe (\ref -> map (\tr -> Tuple (declRefSourceSpan ref) tr) (getTypeRef ref))
    where
    combineEntry (Tuple ss' (Tuple tc dcs1)) (Tuple _ (Tuple _ dcs2)) =
      Tuple ss' (Tuple tc (combDctors dcs1 dcs2))
    combDctors Nothing   _          = Nothing
    combDctors _         Nothing    = Nothing
    combDctors (Just a)  (Just b)   = Just (a <> b)

  groupByTypeName
    :: Array (Tuple SourceSpan (Tuple (ProperName TypeName) (Maybe (Array (ProperName ConstructorName)))))
    -> Array (Array (Tuple SourceSpan (Tuple (ProperName TypeName) (Maybe (Array (ProperName ConstructorName))))))
  groupByTypeName [] = []
  groupByTypeName arr = case Array.uncons arr of
    Nothing -> []
    Just { head: x@(Tuple _ (Tuple tc _)), tail: xs } ->
      let same = Array.takeWhile (\(Tuple _ (Tuple tc' _)) -> tc == tc') xs
          rest = Array.dropWhile (\(Tuple _ (Tuple tc' _)) -> tc == tc') xs
      in Array.cons (Array.cons x same) (groupByTypeName rest)

  foldl1Maybe :: forall a. (a -> a -> a) -> Array a -> Maybe a
  foldl1Maybe _ [] = Nothing
  foldl1Maybe f arr = case Array.uncons arr of
    Nothing -> Nothing
    Just { head, tail } -> Just (Array.foldl f head tail)

  filterTypes
    :: Map (ProperName TypeName) (Tuple (Array (ProperName ConstructorName)) ExportSource)
    -> DeclarationRef
    -> m (Map (ProperName TypeName) (Tuple (Array (ProperName ConstructorName)) ExportSource))
  filterTypes result (TypeRef ss name expDcons) =
    let Exports expsRec = exps
    in case Map.lookup name expsRec.exportedTypes of
      Nothing -> throwError (errorMessage' ss (UnknownExport (TyName name)))
      Just (Tuple dcons src) -> do
        let expDcons' = fromMaybe dcons expDcons
        traverse_ (checkDcon ss name dcons) expDcons'
        pure $ Map.insert name (Tuple expDcons' src) result
  filterTypes result _ = pure result

  checkDcon
    :: SourceSpan
    -> ProperName TypeName
    -> Array (ProperName ConstructorName)
    -> ProperName ConstructorName
    -> m Unit
  checkDcon ss tcon dcons dcon =
    unless (Array.elem dcon dcons)
      (throwError (errorMessage' ss (UnknownExportDataConstructor tcon dcon)))

  filterExport
    :: forall a
     . Ord a
    => (a -> Name)
    -> (DeclarationRef -> Maybe a)
    -> (Exports -> Map a ExportSource)
    -> Map a ExportSource
    -> DeclarationRef
    -> m (Map a ExportSource)
  filterExport toName get fromExps result ref =
    case get ref of
      Just name ->
        case Map.lookup name (fromExps exps) of
          Just source' ->
            let ExportSource srcRec = source'
            in if mn == srcRec.exportSourceDefinedIn
               then pure (Map.insert name source' result)
               else throwError (errorMessage' (declRefSourceSpan ref) (UnknownExport (toName name)))
          Nothing ->
            throwError (errorMessage' (declRefSourceSpan ref) (UnknownExport (toName name)))
      Nothing -> pure result

when :: forall m. Applicative m => Boolean -> m Unit -> m Unit
when true  m = m
when false _ = pure unit

unless :: forall m. Applicative m => Boolean -> m Unit -> m Unit
unless false m = m
unless true  _ = pure unit

void :: forall f a. Functor f => f a -> f Unit
void = map (const unit)
