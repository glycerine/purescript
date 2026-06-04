module Language.PureScript.Sugar.Names.Imports
  ( ImportDef
  , resolveImports
  , resolveModuleImport
  , findImports
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Array as Array
import Data.Foldable (foldM, traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..), snd)

import Language.PureScript.AST.Declarations
  ( Declaration(..)
  , DeclarationRef(..)
  , ErrorMessageHint(..)
  , ExportSource(..)
  , ImportDeclarationType(..)
  , Module(..)
  )
import Language.PureScript.AST.SourcePos (SourceSpan, internalModuleSourceSpan)
import Language.PureScript.Errors
  ( MultipleErrors
  , SimpleErrorMessage(..)
  , addHint
  , errorMessage'
  , rethrow
  )
import Language.PureScript.Names
  ( ModuleName
  , Name(..)
  , ProperName
  , ConstructorName
  , Qualified(..)
  , QualifiedBy(..)
  , byMaybeModuleName
  , byNullSourcePos
  )
import Language.PureScript.Sugar.Names.Env
  ( Env
  , Exports(..)
  , ImportProvenance(..)
  , ImportRecord(..)
  , Imports(..)
  , envModuleExports
  , nullImports
  )

type ImportDef = { ss :: SourceSpan, typ :: ImportDeclarationType, qual :: Maybe ModuleName }

findImports :: Array Declaration -> Map ModuleName (Array ImportDef)
findImports decls = Array.foldr go Map.empty decls
  where
  go (ImportDeclaration (Tuple pos _) mn typ qual) acc =
    Map.alter (\existing -> Just (Array.cons { ss: pos, typ, qual } (fromMaybe [] existing))) mn acc
  go _ acc = acc

resolveImports
  :: forall m
   . MonadError MultipleErrors m
  => Env
  -> Module
  -> m (Tuple Module Imports)
resolveImports env (Module ss coms currentModule decls exps) =
  rethrow (addHint (ErrorInModule currentModule)) do
    let imports  = findImports decls
        imports' = map (\arr -> map (\r -> { ss: r.ss, mTyp: Just r.typ, qual: r.qual }) arr) imports
        selfEntry = [ { ss: internalModuleSourceSpan "<module>", mTyp: Nothing, qual: Nothing } ]
        scope    = Map.insert currentModule selfEntry imports'
    result <- foldM (resolveModuleImport env) nullImports (Map.toUnfoldable scope :: Array _)
    pure (Tuple (Module ss coms currentModule decls exps) result)

resolveModuleImport
  :: forall m
   . MonadError MultipleErrors m
  => Env
  -> Imports
  -> Tuple ModuleName (Array { ss :: SourceSpan, mTyp :: Maybe ImportDeclarationType, qual :: Maybe ModuleName })
  -> m Imports
resolveModuleImport env ie (Tuple mn imps) = foldM go ie imps
  where
  go ie' { ss, mTyp, qual: impQual } = do
    modExports <-
      case Map.lookup mn env of
        Nothing -> throwError (errorMessage' ss (UnknownName (Qualified byNullSourcePos (ModName mn))))
        Just e  -> pure (envModuleExports e)
    let Imports imp = ie'
        impModules  = imp.importedModules
        qualModules = imp.importedQualModules
        ie'' = Imports imp
          { importedModules    = case impQual of
              Nothing -> Set.insert mn impModules
              Just _  -> impModules
          , importedQualModules = case impQual of
              Nothing  -> qualModules
              Just qmn -> Set.insert qmn qualModules
          }
    resolveImport mn modExports ie'' impQual ss mTyp

resolveImport
  :: forall m
   . MonadError MultipleErrors m
  => ModuleName
  -> Exports
  -> Imports
  -> Maybe ModuleName
  -> SourceSpan
  -> Maybe ImportDeclarationType
  -> m Imports
resolveImport importModule exps imps impQual ss mTyp = resolveByType ss mTyp
  where

  resolveByType :: SourceSpan -> Maybe ImportDeclarationType -> m Imports
  resolveByType ss' Nothing =
    importAll ss' (importRef Local)
  resolveByType ss' (Just Implicit) =
    importAll ss' (importRef FromImplicit)
  resolveByType _ (Just (Explicit refs)) = do
    checkRefs false refs
    foldM (importRef FromExplicit) imps refs
  resolveByType ss' (Just (Hiding refs)) = do
    checkRefs true refs
    importAll ss' (importNonHidden refs)

  checkRefs :: Boolean -> Array DeclarationRef -> m Unit
  checkRefs isHiding = traverse_ check
    where
    Exports expsRec = exps
    check (ValueRef ss' name) =
      checkImportExists ss' IdentName expsRec.exportedValues name
    check (ValueOpRef ss' op) =
      checkImportExists ss' ValOpName expsRec.exportedValueOps op
    check (TypeRef ss' name dctors) = do
      checkImportExists ss' TyName (map snd expsRec.exportedTypes) name
      let Tuple allDctors _ = allExportedDataConstructors name
      case dctors of
        Nothing -> pure unit
        Just ds -> traverse_ (checkDctorExists ss' name allDctors) ds
    check (TypeOpRef ss' name) =
      checkImportExists ss' TyOpName expsRec.exportedTypeOps name
    check (TypeClassRef ss' name) =
      checkImportExists ss' TyClassName expsRec.exportedTypeClasses name
    check (ModuleRef ss' name) | isHiding =
      throwError (errorMessage' ss' (ImportHidingModule name))
    check _ = pure unit

  checkImportExists
    :: forall a
     . Ord a
    => SourceSpan
    -> (a -> Name)
    -> Map a _
    -> a
    -> m Unit
  checkImportExists ss' toName exports item =
    when (not (Map.member item exports))
      (throwError (errorMessage' ss' (UnknownImport importModule (toName item))))

  checkDctorExists
    :: SourceSpan
    -> ProperName _
    -> Array (ProperName ConstructorName)
    -> ProperName ConstructorName
    -> m Unit
  checkDctorExists ss' tcon exports dctor =
    unless (Array.elem dctor exports)
      (throwError (errorMessage' ss' (UnknownImportDataConstructor importModule tcon dctor)))

  importNonHidden :: Array DeclarationRef -> Imports -> DeclarationRef -> m Imports
  importNonHidden hidden m ref =
    if isHidden ref then pure m else importRef FromImplicit m ref
    where
    isHidden :: DeclarationRef -> Boolean
    isHidden ref'@(TypeRef _ _ _) = Array.foldl (checkTypeRef ref') false hidden
    isHidden ref'                  = Array.elem ref' hidden

    checkTypeRef :: DeclarationRef -> Boolean -> DeclarationRef -> Boolean
    checkTypeRef _ true _ = true
    checkTypeRef (TypeRef _ _ Nothing)           acc (TypeRef _ _ (Just _))  = acc
    checkTypeRef (TypeRef _ name (Just dctor))   _   (TypeRef _ name' (Just dctor')) =
      name == name' && dctor == dctor'
    checkTypeRef (TypeRef _ name _)              _   (TypeRef _ name' Nothing) = name == name'
    checkTypeRef _ acc _ = acc

  importAll :: SourceSpan -> (Imports -> DeclarationRef -> m Imports) -> m Imports
  importAll ss' importer = do
    let Exports expsRec = exps
    m1 <- foldM (\m (Tuple name (Tuple dctors _)) -> importer m (TypeRef ss' name (Just dctors)))
           imps (Map.toUnfoldable expsRec.exportedTypes :: Array _)
    m2 <- foldM (\m (Tuple name _) -> importer m (TypeOpRef ss' name))
           m1 (Map.toUnfoldable expsRec.exportedTypeOps :: Array _)
    m3 <- foldM (\m (Tuple name _) -> importer m (ValueRef ss' name))
           m2 (Map.toUnfoldable expsRec.exportedValues :: Array _)
    m4 <- foldM (\m (Tuple name _) -> importer m (ValueOpRef ss' name))
           m3 (Map.toUnfoldable expsRec.exportedValueOps :: Array _)
    foldM (\m (Tuple name _) -> importer m (TypeClassRef ss' name))
           m4 (Map.toUnfoldable expsRec.exportedTypeClasses :: Array _)

  importRef :: ImportProvenance -> Imports -> DeclarationRef -> m Imports
  importRef prov (Imports imp) (ValueRef ss' name) = do
    let Exports expsRec = exps
        values' = updateImports imp.importedValues expsRec.exportedValues identity name ss' prov
    pure $ Imports imp { importedValues = values' }
  importRef prov (Imports imp) (ValueOpRef ss' name) = do
    let Exports expsRec = exps
        valueOps' = updateImports imp.importedValueOps expsRec.exportedValueOps identity name ss' prov
    pure $ Imports imp { importedValueOps = valueOps' }
  importRef prov (Imports imp) (TypeRef ss' name dctors) = do
    let Exports expsRec = exps
        types' = updateImports imp.importedTypes (map snd expsRec.exportedTypes) identity name ss' prov
        Tuple dctorNames src = allExportedDataConstructors name
        dctorLookup = Map.fromFoldable (map (\d -> Tuple d src) dctorNames)
        theDctors = fromMaybe dctorNames dctors
    case dctors of
      Nothing -> pure unit
      Just ds -> traverse_ (checkDctorExists ss' name dctorNames) ds
    let dctors' = Array.foldl (\m d -> updateImports m dctorLookup identity d ss' prov)
                    imp.importedDataConstructors theDctors
    pure $ Imports imp { importedTypes = types', importedDataConstructors = dctors' }
  importRef prov (Imports imp) (TypeOpRef ss' name) = do
    let Exports expsRec = exps
        ops' = updateImports imp.importedTypeOps expsRec.exportedTypeOps identity name ss' prov
    pure $ Imports imp { importedTypeOps = ops' }
  importRef prov (Imports imp) (TypeClassRef ss' name) = do
    let Exports expsRec = exps
        typeClasses' = updateImports imp.importedTypeClasses expsRec.exportedTypeClasses identity name ss' prov
    pure $ Imports imp { importedTypeClasses = typeClasses' }
  importRef _ _ _ = pure imps

  allExportedDataConstructors
    :: ProperName _
    -> Tuple (Array (ProperName ConstructorName)) ExportSource
  allExportedDataConstructors name =
    let Exports expsRec = exps
    in case Map.lookup name expsRec.exportedTypes of
      Just (Tuple dctors src) -> Tuple dctors src
      Nothing -> Tuple [] (ExportSource { exportSourceImportedFrom: Nothing, exportSourceDefinedIn: importModule })

  updateImports
    :: forall a b
     . Ord a
    => Map (Qualified a) (Array (ImportRecord a))
    -> Map a b
    -> (b -> ExportSource)
    -> a
    -> SourceSpan
    -> ImportProvenance
    -> Map (Qualified a) (Array (ImportRecord a))
  updateImports imps' exps' expName name ss' prov =
    let
      src = case Map.lookup name exps' of
        Just v  -> expName v
        Nothing -> ExportSource { exportSourceImportedFrom: Nothing, exportSourceDefinedIn: importModule }
      ExportSource srcRec = src
      rec = ImportRecord
        { importName:         Qualified (ByModuleName importModule) name
        , importSourceModule: srcRec.exportSourceDefinedIn
        , importSourceSpan:   ss'
        , importProvenance:   prov
        }
    in
      Map.alter
        (\currNames -> Just (Array.cons rec (fromMaybe [] currNames)))
        (Qualified (byMaybeModuleName impQual) name)
        imps'

when :: forall m. Applicative m => Boolean -> m Unit -> m Unit
when true  m = m
when false _ = pure unit

unless :: forall m. Applicative m => Boolean -> m Unit -> m Unit
unless false m = m
unless true  _ = pure unit
