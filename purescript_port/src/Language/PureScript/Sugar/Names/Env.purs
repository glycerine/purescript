module Language.PureScript.Sugar.Names.Env
  ( ImportRecord(..)
  , ImportProvenance(..)
  , ImportMap
  , Imports(..)
  , nullImports
  , Exports(..)
  , nullExports
  , Env
  , primEnv
  , primExports
  , envModuleExports
  , ExportMode(..)
  , exportType
  , exportTypeOp
  , exportTypeClass
  , exportValue
  , exportValueOp
  , checkImportConflicts
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Writer.Class (class MonadWriter, tell)
import Data.Array as Array
import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)

import Language.PureScript.AST.Declarations (ExportSource(..))
import Language.PureScript.AST.SourcePos (SourceSpan, internalModuleSourceSpan, nullSourceSpan)
import Language.PureScript.Environment
  ( TypeClassData
  , primTypes
  , primClasses
  , primBooleanTypes
  , primCoerceTypes
  , primCoerceClasses
  , primOrderingTypes
  , primRowTypes
  , primRowClasses
  , primRowListTypes
  , primRowListClasses
  , primSymbolTypes
  , primSymbolClasses
  , primIntTypes
  , primIntClasses
  , primTypeErrorTypes
  , primTypeErrorClasses
  )
import Language.PureScript.Errors
  ( MultipleErrors
  , SimpleErrorMessage(..)
  , errorMessage
  , errorMessage'
  )
import Language.PureScript.Names
  ( ClassName
  , ConstructorName
  , Ident
  , ModuleName(..)
  , Name(..)
  , OpName
  , ProperName(..)
  , Qualified(..)
  , QualifiedBy(..)
  , TypeName
  , ValueOpName
  , TypeOpName
  , coerceProperName
  , disqualify
  , getQual
  )
import Language.PureScript.Constants.Prim as C

data ImportProvenance
  = FromImplicit
  | FromExplicit
  | Local
  | Prim

derive instance eqImportProvenance :: Eq ImportProvenance
derive instance ordImportProvenance :: Ord ImportProvenance

instance showImportProvenance :: Show ImportProvenance where
  show FromImplicit = "FromImplicit"
  show FromExplicit = "FromExplicit"
  show Local        = "Local"
  show Prim         = "Prim"

data ImportRecord a = ImportRecord
  { importName         :: Qualified a
  , importSourceModule :: ModuleName
  , importSourceSpan   :: SourceSpan
  , importProvenance   :: ImportProvenance
  }

derive instance eqImportRecord :: (Eq a) => Eq (ImportRecord a)
derive instance ordImportRecord :: (Ord a) => Ord (ImportRecord a)

instance showImportRecord :: Show a => Show (ImportRecord a) where
  show (ImportRecord r) =
    "(ImportRecord { name: " <> show r.importName <> ", provenance: " <> show r.importProvenance <> " })"

type ImportMap a = Map (Qualified a) (Array (ImportRecord a))

data Imports = Imports
  { importedTypes             :: ImportMap (ProperName TypeName)
  , importedTypeOps           :: ImportMap (OpName TypeOpName)
  , importedDataConstructors  :: ImportMap (ProperName ConstructorName)
  , importedTypeClasses       :: ImportMap (ProperName ClassName)
  , importedValues            :: ImportMap Ident
  , importedValueOps          :: ImportMap (OpName ValueOpName)
  , importedModules           :: Set ModuleName
  , importedQualModules       :: Set ModuleName
  , importedKinds             :: ImportMap (ProperName TypeName)
  }

instance showImports :: Show Imports where
  show _ = "<Imports>"

nullImports :: Imports
nullImports = Imports
  { importedTypes:            Map.empty
  , importedTypeOps:          Map.empty
  , importedDataConstructors: Map.empty
  , importedTypeClasses:      Map.empty
  , importedValues:           Map.empty
  , importedValueOps:         Map.empty
  , importedModules:          Set.empty
  , importedQualModules:      Set.empty
  , importedKinds:            Map.empty
  }

data Exports = Exports
  { exportedTypes       :: Map (ProperName TypeName) (Tuple (Array (ProperName ConstructorName)) ExportSource)
  , exportedTypeOps     :: Map (OpName TypeOpName) ExportSource
  , exportedTypeClasses :: Map (ProperName ClassName) ExportSource
  , exportedValues      :: Map Ident ExportSource
  , exportedValueOps    :: Map (OpName ValueOpName) ExportSource
  }

instance showExports :: Show Exports where
  show _ = "<Exports>"

nullExports :: Exports
nullExports = Exports
  { exportedTypes:       Map.empty
  , exportedTypeOps:     Map.empty
  , exportedTypeClasses: Map.empty
  , exportedValues:      Map.empty
  , exportedValueOps:    Map.empty
  }

type Env = Map ModuleName (Tuple (Tuple SourceSpan Imports) Exports)

envModuleExports :: forall a b c. Tuple (Tuple a b) c -> c
envModuleExports = snd

primExports :: Exports
primExports = mkPrimExports primTypes primClasses

primBooleanExports :: Exports
primBooleanExports = mkPrimExports primBooleanTypes Map.empty

primCoerceExports :: Exports
primCoerceExports = mkPrimExports primCoerceTypes primCoerceClasses

primOrderingExports :: Exports
primOrderingExports = mkPrimExports primOrderingTypes Map.empty

primRowExports :: Exports
primRowExports = mkPrimExports primRowTypes primRowClasses

primRowListExports :: Exports
primRowListExports = mkPrimExports primRowListTypes primRowListClasses

primSymbolExports :: Exports
primSymbolExports = mkPrimExports primSymbolTypes primSymbolClasses

primIntExports :: Exports
primIntExports = mkPrimExports primIntTypes primIntClasses

primTypeErrorExports :: Exports
primTypeErrorExports = mkPrimExports primTypeErrorTypes primTypeErrorClasses

mkPrimExports
  :: forall a b
   . Map (Qualified (ProperName TypeName)) a
  -> Map (Qualified (ProperName ClassName)) b
  -> Exports
mkPrimExports ts cs = Exports
  { exportedTypes:       Map.fromFoldable (mapMaybe mkTypeEntry (Array.fromFoldable (Map.keys ts)))
  , exportedTypeOps:     Map.empty
  , exportedTypeClasses: Map.fromFoldable (mapMaybe mkClassEntry (Array.fromFoldable (Map.keys cs)))
  , exportedValues:      Map.empty
  , exportedValueOps:    Map.empty
  }
  where
  mkTypeEntry :: Qualified (ProperName TypeName) -> Maybe (Tuple (ProperName TypeName) (Tuple (Array (ProperName ConstructorName)) ExportSource))
  mkTypeEntry (Qualified (ByModuleName mn) name) =
    Just (Tuple name (Tuple [] (primExportSource mn)))
  mkTypeEntry _ = Nothing

  mkClassEntry :: Qualified (ProperName ClassName) -> Maybe (Tuple (ProperName ClassName) ExportSource)
  mkClassEntry (Qualified (ByModuleName mn) name) =
    Just (Tuple name (primExportSource mn))
  mkClassEntry _ = Nothing

  primExportSource :: ModuleName -> ExportSource
  primExportSource mn = ExportSource
    { exportSourceImportedFrom: Nothing
    , exportSourceDefinedIn: mn
    }

primEnv :: Env
primEnv = Map.fromFoldable
  [ Tuple C.mPrim
      (Tuple (Tuple (internalModuleSourceSpan "<Prim>") nullImports) primExports)
  , Tuple C.mPrimBoolean
      (Tuple (Tuple (internalModuleSourceSpan "<Prim.Boolean>") nullImports) primBooleanExports)
  , Tuple C.mPrimCoerce
      (Tuple (Tuple (internalModuleSourceSpan "<Prim.Coerce>") nullImports) primCoerceExports)
  , Tuple C.mPrimOrdering
      (Tuple (Tuple (internalModuleSourceSpan "<Prim.Ordering>") nullImports) primOrderingExports)
  , Tuple C.mPrimRow
      (Tuple (Tuple (internalModuleSourceSpan "<Prim.Row>") nullImports) primRowExports)
  , Tuple C.mPrimRowList
      (Tuple (Tuple (internalModuleSourceSpan "<Prim.RowList>") nullImports) primRowListExports)
  , Tuple C.mPrimSymbol
      (Tuple (Tuple (internalModuleSourceSpan "<Prim.Symbol>") nullImports) primSymbolExports)
  , Tuple C.mPrimInt
      (Tuple (Tuple (internalModuleSourceSpan "<Prim.Int>") nullImports) primIntExports)
  , Tuple C.mPrimTypeError
      (Tuple (Tuple (internalModuleSourceSpan "<Prim.TypeError>") nullImports) primTypeErrorExports)
  ]

data ExportMode = Internal | ReExport

derive instance eqExportMode :: Eq ExportMode

instance showExportMode :: Show ExportMode where
  show Internal = "Internal"
  show ReExport = "ReExport"

exportType
  :: forall m
   . MonadError MultipleErrors m
  => SourceSpan
  -> ExportMode
  -> Exports
  -> ProperName TypeName
  -> Array (ProperName ConstructorName)
  -> ExportSource
  -> m Exports
exportType ss exportMode (Exports exps) name dctors src = do
  let exTypes   = exps.exportedTypes
      exClasses = exps.exportedTypeClasses
      dctorNameCounts = Map.toUnfoldable (Map.fromFoldableWith (+) (map (\d -> Tuple d 1) dctors)) :: Array (Tuple (ProperName ConstructorName) Int)
  for_ dctorNameCounts \(Tuple dctorName count) ->
    when (count > 1) $
      throwDeclConflict (DctorName dctorName) (DctorName dctorName)
  case exportMode of
    Internal -> do
      when (Map.member name exTypes) $
        throwDeclConflict (TyName name) (TyName name)
      when (Map.member (coerceProperName name) exClasses) $
        throwDeclConflict (TyName name) (TyClassName (coerceProperName name))
      for_ dctors \dctor -> do
        when (Array.any (\(Tuple _ (Tuple ds _)) -> Array.elem dctor ds) (Map.toUnfoldable exTypes :: Array _)) $
          throwDeclConflict (DctorName dctor) (DctorName dctor)
        when (Map.member (coerceProperName dctor) exClasses) $
          throwDeclConflict (DctorName dctor) (TyClassName (coerceProperName dctor))
    ReExport -> do
      let mn = ((\(ExportSource s) -> s.exportSourceDefinedIn) src)
      case Map.lookup (coerceProperName name) exClasses of
        Just src' ->
          let mn' = (\(ExportSource s) -> s.exportSourceDefinedIn) src'
          in throwExportConflict' ss mn mn' (TyName name) (TyClassName (coerceProperName name))
        Nothing -> pure unit
      case Map.lookup name exTypes of
        Just (Tuple _ src') ->
          let mn' = (\(ExportSource s) -> s.exportSourceDefinedIn) src'
          in when (mn /= mn') $
               throwExportConflict ss mn mn' (TyName name)
        Nothing -> pure unit
      for_ dctors \dctor ->
        case Array.findMap (\(Tuple _ (Tuple ds src')) -> if Array.elem dctor ds then Just src' else Nothing) (Map.toUnfoldable exTypes :: Array _) of
          Just src' ->
            let mn' = (\(ExportSource s) -> s.exportSourceDefinedIn) src'
            in when (mn /= mn') $
                 throwExportConflict ss mn mn' (DctorName dctor)
          Nothing -> pure unit
  let updateOrInsert Nothing = Just (Tuple dctors src)
      updateOrInsert (Just (Tuple dctors' _)) = Just (Tuple (dctors' <> dctors) src)
  pure $ Exports exps { exportedTypes = Map.alter updateOrInsert name exTypes }

exportTypeOp
  :: forall m
   . MonadError MultipleErrors m
  => SourceSpan
  -> Exports
  -> OpName TypeOpName
  -> ExportSource
  -> m Exports
exportTypeOp ss (Exports exps) op src = do
  typeOps <- addExport ss TyOpName op src exps.exportedTypeOps
  pure $ Exports exps { exportedTypeOps = typeOps }

exportTypeClass
  :: forall m
   . MonadError MultipleErrors m
  => SourceSpan
  -> ExportMode
  -> Exports
  -> ProperName ClassName
  -> ExportSource
  -> m Exports
exportTypeClass ss exportMode (Exports exps) name src = do
  let exTypes = exps.exportedTypes
  when (exportMode == Internal) do
    when (Map.member (coerceProperName name) exTypes) $
      throwDeclConflict (TyClassName name) (TyName (coerceProperName name))
    when (Array.any (\(Tuple _ (Tuple ds _)) -> Array.elem (coerceProperName name) ds) (Map.toUnfoldable exTypes :: Array _)) $
      throwDeclConflict (TyClassName name) (DctorName (coerceProperName name))
  classes <- addExport ss TyClassName name src exps.exportedTypeClasses
  pure $ Exports exps { exportedTypeClasses = classes }

exportValue
  :: forall m
   . MonadError MultipleErrors m
  => SourceSpan
  -> Exports
  -> Ident
  -> ExportSource
  -> m Exports
exportValue ss (Exports exps) name src = do
  values <- addExport ss IdentName name src exps.exportedValues
  pure $ Exports exps { exportedValues = values }

exportValueOp
  :: forall m
   . MonadError MultipleErrors m
  => SourceSpan
  -> Exports
  -> OpName ValueOpName
  -> ExportSource
  -> m Exports
exportValueOp ss (Exports exps) op src = do
  valueOps <- addExport ss ValOpName op src exps.exportedValueOps
  pure $ Exports exps { exportedValueOps = valueOps }

addExport
  :: forall m a
   . MonadError MultipleErrors m
  => Ord a
  => SourceSpan
  -> (a -> Name)
  -> a
  -> ExportSource
  -> Map a ExportSource
  -> m (Map a ExportSource)
addExport ss toName name src exports =
  case Map.lookup name exports of
    Just src' ->
      let mn  = (\(ExportSource s) -> s.exportSourceDefinedIn) src
          mn' = (\(ExportSource s) -> s.exportSourceDefinedIn) src'
      in if mn == mn'
           then pure exports
           else throwExportConflict ss mn mn' (toName name)
    Nothing ->
      pure $ Map.insert name src exports

throwDeclConflict
  :: forall m a
   . MonadError MultipleErrors m
  => Name
  -> Name
  -> m a
throwDeclConflict new existing =
  throwError (errorMessage (DeclConflict new existing))

throwExportConflict
  :: forall m a
   . MonadError MultipleErrors m
  => SourceSpan
  -> ModuleName
  -> ModuleName
  -> Name
  -> m a
throwExportConflict ss new existing name =
  throwExportConflict' ss new existing name name

throwExportConflict'
  :: forall m a
   . MonadError MultipleErrors m
  => SourceSpan
  -> ModuleName
  -> ModuleName
  -> Name
  -> Name
  -> m a
throwExportConflict' ss new existing newName existingName =
  throwError (errorMessage' ss
    (ExportConflict (Qualified (ByModuleName new) newName)
                    (Qualified (ByModuleName existing) existingName)))

checkImportConflicts
  :: forall m a
   . MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => Ord a
  => SourceSpan
  -> ModuleName
  -> (a -> Name)
  -> Array (ImportRecord a)
  -> m (Tuple ModuleName ModuleName)
checkImportConflicts ss currentModule toName xs =
  let
    byOrig      = Array.sortBy (\(ImportRecord a) (ImportRecord b) -> compare a.importSourceModule b.importSourceModule) xs
    groups      = groupBySourceModule byOrig
    nonImplicit = Array.filter (\(ImportRecord r) -> r.importProvenance /= FromImplicit) xs
    name        = case Array.head xs of
      Nothing -> TyName (ProperName "unknown")
      Just (ImportRecord r) -> toName (disqualify r.importName)
    conflictModules = mapMaybe (\g -> case Array.head g of
      Just (ImportRecord r) -> getQual r.importName
      Nothing -> Nothing) groups
  in
    if Array.length groups > 1
    then case nonImplicit of
      [ ImportRecord r ] -> do
        let mnNew = case r.importName of
              Qualified (ByModuleName mn) _ -> mn
              _ -> currentModule
            warningModule = if mnNew == currentModule then Nothing else Just mnNew
            ss' = case Array.find (\(ImportRecord ir) -> ir.importProvenance == FromImplicit) xs of
              Just (ImportRecord ir) -> ir.importSourceSpan
              Nothing -> nullSourceSpan
        tell (errorMessage' ss' (ScopeShadowing name warningModule (Array.delete mnNew conflictModules)))
        pure (Tuple mnNew r.importSourceModule)
      _ -> throwError (errorMessage' ss (ScopeConflict name conflictModules))
    else
      case Array.head byOrig of
        Just (ImportRecord r) ->
          case r.importName of
            Qualified (ByModuleName mnNew) _ ->
              pure (Tuple mnNew r.importSourceModule)
            _ -> throwError (errorMessage (InternalCompilerError "checkImportConflicts" "ImportRecord should be qualified"))
        Nothing -> throwError (errorMessage (InternalCompilerError "checkImportConflicts" "No imports found"))

-- Group consecutive records with the same importSourceModule
groupBySourceModule :: forall a. Array (ImportRecord a) -> Array (Array (ImportRecord a))
groupBySourceModule [] = []
groupBySourceModule arr = case Array.uncons arr of
  Nothing -> []
  Just { head: x@(ImportRecord rx), tail: xs } ->
    let same = Array.takeWhile (\(ImportRecord r) -> r.importSourceModule == rx.importSourceModule) xs
        rest = Array.dropWhile (\(ImportRecord r) -> r.importSourceModule == rx.importSourceModule) xs
    in Array.cons (Array.cons x same) (groupBySourceModule rest)

for_ :: forall m a. Monad m => Array a -> (a -> m Unit) -> m Unit
for_ xs f = void (Array.foldM (\_ a -> f a) unit xs)

when :: forall m. Applicative m => Boolean -> m Unit -> m Unit
when true  m = m
when false _ = pure unit
