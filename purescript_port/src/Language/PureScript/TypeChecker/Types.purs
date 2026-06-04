-- |
-- This module implements the type checker for PureScript values.
-- Port of Language.PureScript.TypeChecker.Types from Haskell.
--
module Language.PureScript.TypeChecker.Types
  ( BindingGroupType(..)
  , typesOf
  , checkTypeKind
  ) where

import Prelude

import Control.Monad (when, unless)
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.State.Class (class MonadState, gets, get)
import Control.Monad.Supply.Class (class MonadSupply, freshIdent)
import Control.Monad.Writer.Class (class MonadWriter, tell)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_, foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set as Set
import Data.String as String
import Data.Traversable (traverse, for)
import Data.Tuple (Tuple(..), fst, snd)
import Data.List.NonEmpty as NEL

import Language.PureScript.AST.Binders (Binder(..), binderNames)
import Language.PureScript.AST.Declarations
  ( CaseAlternative(..)
  , Declaration(..)
  , ErrorMessageHint(..)
  , Expr(..)
  , Guard(..)
  , GuardedExpr(..)
  , ValueDeclarationData(..)
  )
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos
  ( SourceAnn
  , SourcePos(..)
  , SourceSpan(..)
  , nullSourceAnn
  , nullSourceSpan
  )
import Language.PureScript.Environment
  ( Environment(..)
  , FunctionalDependency(..)
  , NameKind(..)
  , NameVisibility(..)
  , TypeClassData(..)
  , function
  , tyRecord
  )
import Language.PureScript.Errors
  ( ErrorMessage(..)
  , MultipleErrors
  , SimpleErrorMessage(..)
  , errorMessage
  , errorMessage'
  , internalCompilerError
  , onErrorMessages
  , parU
  )
import Language.PureScript.Label (Label(..))
import Language.PureScript.Names
  ( ClassName
  , ConstructorName
  , Ident(..)
  , ModuleName(..)
  , Name(..)
  , ProperName(..)
  , Qualified(..)
  , QualifiedBy(..)
  , TypeName
  , byMaybeModuleName
  , byNullSourcePos
  , coerceProperName
  )
import Language.PureScript.PSString (PSString)
import Language.PureScript.TypeChecker.Deriving (deriveInstance)
import Language.PureScript.TypeChecker.Entailment
  ( InstanceContext
  , newDictionaries
  , replaceTypeClassDictionaries
  )
import Language.PureScript.TypeChecker.Kinds
  ( checkConstraint
  , checkKind
  , kindOf
  , kindOfWithScopedVars
  , kindType
  , unknownsWithKinds
  , unifyKinds'
  )
import Language.PureScript.TypeChecker.Monad
  ( CheckState(..)
  , Substitution(..)
  , bindLocalVariables
  , bindNames
  , capturingSubstitution
  , checkVisibility
  , getEnv
  , getHints
  , getLocalContext
  , getTypeClassDictionaries
  , guardWith
  , insertUnkName
  , lookupUnkName
  , lookupVariable
  , makeBindingGroupVisible
  , unsafeCheckCurrentModule
  , warnAndRethrowWithPositionTC
  , withBindingGroupVisible
  , withErrorMessageHint
  , withFreshSubstitution
  , withScopedTypeVars
  , withTypeClassDictionaries
  , withoutWarnings
  )
import Language.PureScript.TypeChecker.Skolems
  ( introduceSkolemScope
  , newSkolemConstant
  , newSkolemScope
  , skolemEscapeCheck
  , skolemize
  , skolemizeTypesInValue
  )
import Language.PureScript.TypeChecker.Subsumption (subsumes)
import Language.PureScript.TypeChecker.Synonyms (replaceAllTypeSynonyms)
import Language.PureScript.TypeChecker.Unify
  ( freshTypeWithKind
  , replaceTypeWildcards
  , substituteType
  , unknownsInType
  , unifyTypes
  , varIfUnknown
  )
import Language.PureScript.Types
  ( Constraint(..)
  , RowListItem(..)
  , SourceConstraint
  , SourceType
  , Type(..)
  , TypeVarVisibility(..)
  , replaceTypeVars
  , rowFromList
  , rowToList
  , srcConstrainedType
  , srcConstraint
  , srcKindApp
  , srcRCons
  , srcREmpty
  , srcRowListItem
  , srcTypeApp
  , srcTypeConstructor
  )

-- ─── Primitive types ─────────────────────────────────────────────────────────

primTy :: String -> SourceType
primTy name = srcTypeConstructor (Qualified (ByModuleName (ModuleName "Prim")) (ProperName name))

tyFunction :: SourceType
tyFunction = primTy "Function"

tyArray :: SourceType
tyArray = primTy "Array"

tyInt :: SourceType
tyInt = primTy "Int"

tyNumber :: SourceType
tyNumber = primTy "Number"

tyString :: SourceType
tyString = primTy "String"

tyChar :: SourceType
tyChar = primTy "Char"

tyBoolean :: SourceType
tyBoolean = primTy "Boolean"

kindRowOf :: SourceType -> SourceType
kindRowOf = srcTypeApp (primTy "Row")

-- ─── Utilities ───────────────────────────────────────────────────────────────

isMonoType :: SourceType -> Boolean
isMonoType (ForAll _ _ _ _ _ _) = false
isMonoType (ParensInType _ t)    = isMonoType t
isMonoType _                     = true

isDictTypeName :: forall a. ProperName a -> Boolean
isDictTypeName (ProperName s) = isJust (String.stripSuffix (String.Pattern "$Dict") s)

-- ─── BindingGroupType ─────────────────────────────────────────────────────────

data BindingGroupType
  = RecursiveBindingGroup
  | NonRecursiveBindingGroup

derive instance eqBindingGroupType :: Eq BindingGroupType
derive instance ordBindingGroupType :: Ord BindingGroupType

instance showBindingGroupType :: Show BindingGroupType where
  show RecursiveBindingGroup    = "RecursiveBindingGroup"
  show NonRecursiveBindingGroup = "NonRecursiveBindingGroup"

-- ─── Internal types ──────────────────────────────────────────────────────────

data TypedValue' = TypedValue' Boolean Expr SourceType

tvToExpr :: TypedValue' -> Expr
tvToExpr (TypedValue' c e t) = TypedValue c e t

-- ─── checkTypeKind ───────────────────────────────────────────────────────────

checkTypeKind
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => SourceType
  -> SourceType
  -> m Unit
checkTypeKind _ty kind = unifyKinds' kind kindType

-- ─── SplitBindingGroup ───────────────────────────────────────────────────────

data SplitBindingGroup = SplitBindingGroup
  { splitUntyped :: Array (Tuple (Tuple SourceAnn Ident) (Tuple Expr SourceType))
  , splitTyped   :: Array (Tuple (Tuple SourceAnn Ident) (Tuple (Tuple Expr (Array (Tuple String SourceType))) (Tuple SourceType Boolean)))
  , splitDict    :: Map (Qualified Ident) (Tuple (Tuple SourceType NameKind) NameVisibility)
  }

-- ─── lookupTypeClass ─────────────────────────────────────────────────────────

lookupTypeClass
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => Qualified (ProperName ClassName)
  -> m TypeClassData
lookupTypeClass name = do
  env <- getEnv
  let Environment e = env
  case Map.lookup name e.typeClasses of
    Nothing -> internalCompilerError "lookupTypeClass: type class not found"
    Just tc -> pure tc

-- ─── typesOf ─────────────────────────────────────────────────────────────────

typesOf
  :: forall m
   . MonadSupply m
  => MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => BindingGroupType
  -> ModuleName
  -> Array (Tuple (Tuple SourceAnn Ident) Expr)
  -> m (Array (Tuple (Tuple SourceAnn Ident) (Tuple Expr SourceType)))
typesOf bindingGroupType moduleName vals = withFreshSubstitution do
  Tuple tys wInfer <- capturingSubstitution tidyUp do
    Tuple (SplitBindingGroup grp) w <- withoutWarnings $
      typeDictionaryForBindingGroup (Just moduleName) vals
    ds1 <- parU grp.splitTyped \e ->
      withoutWarnings $ checkTypedBindingGroupElement moduleName e grp.splitDict
    ds2 <- traverse (\e -> withoutWarnings $ typeForBindingGroupElement e grp.splitDict)
             grp.splitUntyped
    pure (Tuple (map (Tuple false) ds1 <> map (Tuple true) ds2) w)

  inferred <- traverse (processResult) tys

  finalState <- get
  let CheckState fs = finalState
  tell (onErrorMessages (replaceTypesInError fs.checkSubstitution) wInfer)
  for_ tys \(Tuple _ (Tuple _ w)) ->
    tell (onErrorMessages (replaceTypesInError fs.checkSubstitution) w)

  pure (map fst inferred)
  where
  processResult
    :: Tuple Boolean (Tuple (Tuple (Tuple SourceAnn Ident) (Tuple Expr SourceType)) MultipleErrors)
    -> m (Tuple (Tuple (Tuple SourceAnn Ident) (Tuple Expr SourceType)) (Array (Tuple Ident (Tuple InstanceContext SourceConstraint))))
  processResult (Tuple shouldGeneralize (Tuple (Tuple sai (Tuple val ty)) _)) = do
    Tuple val' unsolved <- replaceTypeClassDictionaries shouldGeneralize val
    currentSubst <- gets \(CheckState s) -> s.checkSubstitution
    let ty'  = substituteType currentSubst ty
        ty'' = constrain unsolved ty'
    let unsolvedInTy'' = Array.nub (map snd (unknownsInType ty''))
    unsolvedTypeVarsWithKinds <- unknownsWithKinds unsolvedInTy''
    let unsolvedTypeVars = Array.nub (map snd (unknownsInType ty'))
    generalized <- varIfUnknown unsolvedTypeVarsWithKinds ty''

    let Tuple ss _        = sai
        Tuple ssSpan _    = ss
        ident             = snd sai

    when shouldGeneralize do
      tell $ errorMessage' ssSpan (MissingTypeDeclaration ident generalized)
      when (bindingGroupType == RecursiveBindingGroup && not (Array.null unsolved)) $
        throwError $ errorMessage' ssSpan (CannotGeneralizeRecursiveFunction ident generalized)
      conData <- traverse (\(Tuple _ (Tuple _ con)) -> do
        let Constraint c = con
        TypeClassData tcData <- lookupTypeClass c.constraintClass
        let unknownsForArg = map (\arg -> Set.fromFoldable (map snd (unknownsInType arg))) c.constraintArgs
        pure (Tuple tcData.typeClassDependencies unknownsForArg)
        ) unsolved
      let solveFrom determined =
            let solved = solve1 determined
            in if Set.subset solved determined then determined
               else solveFrom (Set.union determined solved)
          solve1 determined = Array.foldl Set.union Set.empty do
            Tuple tcDeps conArgUnknowns <- conData
            FunctionalDependency fd <- tcDeps
            let unknownsDetermined i = case Array.index conArgUnknowns i of
                  Nothing -> false
                  Just unks -> Set.subset unks determined
            if Array.all unknownsDetermined fd.fdDeterminers
              then Array.mapMaybe (\i -> Array.index conArgUnknowns i) fd.fdDetermined
              else []
          determinedFromType = Set.fromFoldable unsolvedTypeVars
          constraintTypeVars = Array.foldl
            (\acc (Tuple _ conArgUs) -> Set.union acc (Array.foldl Set.union Set.empty conArgUs))
            Set.empty conData
          solved = solveFrom determinedFromType
          unsolvedVars = Set.difference constraintTypeVars solved

      unsolvedVarNames <- traverse (\i -> do
          mn <- lookupUnkName i
          pure (Tuple (fromMaybe "t" mn) i)
        ) (Array.fromFoldable unsolvedVars)

      unless (Set.isEmpty unsolvedVars) $
        throwError
          $ onErrorMessages (replaceTypesInError currentSubst)
          $ errorMessage' ssSpan (AmbiguousTypeVariables generalized unsolvedVarNames)

    skolemEscapeCheck val'
    let wrappedVal = foldr (\(Tuple x _) acc -> Abs (VarBinder nullSourceSpan x) acc) val' unsolved
    pure (Tuple (Tuple sai (Tuple wrappedVal generalized)) unsolved)

  constrain :: Array (Tuple Ident (Tuple InstanceContext SourceConstraint)) -> SourceType -> SourceType
  constrain cs ty = foldr (\(Tuple _ (Tuple _ con)) t -> srcConstrainedType con t) ty cs

  tidyUp
    :: Tuple (Array (Tuple Boolean (Tuple (Tuple (Tuple SourceAnn Ident) (Tuple Expr SourceType)) MultipleErrors))) MultipleErrors
    -> Substitution
    -> Tuple (Array (Tuple Boolean (Tuple (Tuple (Tuple SourceAnn Ident) (Tuple Expr SourceType)) MultipleErrors))) MultipleErrors
  tidyUp (Tuple ts w) sub = Tuple
    (map (\(Tuple b (Tuple (Tuple sai (Tuple expr ty)) errs)) ->
      Tuple b (Tuple (Tuple sai (Tuple expr (substituteType sub ty))) errs)) ts)
    w

  replaceTypesInError :: Substitution -> ErrorMessage -> ErrorMessage
  replaceTypesInError _sub msg = msg

-- ─── typeDictionaryForBindingGroup ───────────────────────────────────────────

typeDictionaryForBindingGroup
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => Maybe ModuleName
  -> Array (Tuple (Tuple SourceAnn Ident) Expr)
  -> m SplitBindingGroup
typeDictionaryForBindingGroup moduleName vals = do
  let eithers   = map splitTypeAnnotation vals
      untyped   = Array.mapMaybe leftOf eithers
      typed     = Array.mapMaybe rightOf eithers

  typedResults <- for typed \(Tuple sai (Tuple expr (Tuple ty checkType))) -> do
    (Tuple (Tuple args elabTy) _kind :: Tuple (Tuple (Array (Tuple String SourceType)) SourceType) SourceType) <- kindOfWithScopedVars ty
    checkTypeKind ty _kind
    elabTy' <- replaceTypeWildcards elabTy
    pure (Tuple (Tuple sai elabTy') (Tuple sai (Tuple (Tuple expr args) (Tuple elabTy' checkType))))
  let typedDict = map fst typedResults
      typed'    = map snd typedResults

  untypedResults <- for untyped \(Tuple sai expr) -> do
    ty <- freshTypeWithKind kindType
    pure (Tuple (Tuple sai ty) (Tuple sai (Tuple expr ty)))
  let untypedDict = map fst untypedResults
      untyped'    = map snd untypedResults

  let allDictEntries = typedDict <> untypedDict
      dict = Map.fromFoldable $ map (\(Tuple sai ty) ->
        let Tuple ss _  = sai
            Tuple ssSpan _ = ss
            ident = snd sai
            qb    = case moduleName of
                      Just mn -> ByModuleName mn
                      Nothing -> byNullSourcePos
        in Tuple (Qualified qb ident) (Tuple (Tuple ty Private) Undefined)
        ) allDictEntries

  pure $ SplitBindingGroup
    { splitUntyped: untyped'
    , splitTyped:   typed'
    , splitDict:    dict
    }
  where
  splitTypeAnnotation
    :: Tuple (Tuple SourceAnn Ident) Expr
    -> Either (Tuple (Tuple SourceAnn Ident) Expr)
               (Tuple (Tuple SourceAnn Ident) (Tuple Expr (Tuple SourceType Boolean)))
  splitTypeAnnotation (Tuple a (TypedValue checkType value ty)) =
    Right (Tuple a (Tuple value (Tuple ty checkType)))
  splitTypeAnnotation (Tuple a (PositionedValue pos c value)) =
    case splitTypeAnnotation (Tuple a value) of
      Left  (Tuple a' v)                       -> Left  (Tuple a' (PositionedValue pos c v))
      Right (Tuple a' (Tuple e (Tuple t b)))   -> Right (Tuple a' (Tuple (PositionedValue pos c e) (Tuple t b)))
  splitTypeAnnotation x = Left x

  leftOf  :: forall a b. Either a b -> Maybe a
  leftOf  (Left x)  = Just x
  leftOf  (Right _) = Nothing

  rightOf :: forall a b. Either a b -> Maybe b
  rightOf (Right x) = Just x
  rightOf (Left _)  = Nothing

-- ─── checkTypedBindingGroupElement ───────────────────────────────────────────

checkTypedBindingGroupElement
  :: forall m
   . MonadSupply m
  => MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => ModuleName
  -> Tuple (Tuple SourceAnn Ident) (Tuple (Tuple Expr (Array (Tuple String SourceType))) (Tuple SourceType Boolean))
  -> Map (Qualified Ident) (Tuple (Tuple SourceType NameKind) NameVisibility)
  -> m (Tuple (Tuple SourceAnn Ident) (Tuple Expr SourceType))
checkTypedBindingGroupElement mn (Tuple ident (Tuple (Tuple val args) (Tuple ty checkType))) dict = do
  ty' <- (introduceSkolemScope <=< replaceAllTypeSynonyms) ty
  val' <- if checkType
    then withScopedTypeVars mn args $ bindNames dict $ check val ty'
    else pure (TypedValue' false val ty')
  pure (Tuple ident (Tuple (tvToExpr val') ty'))

-- ─── typeForBindingGroupElement ──────────────────────────────────────────────

typeForBindingGroupElement
  :: forall m
   . MonadSupply m
  => MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => Tuple (Tuple SourceAnn Ident) (Tuple Expr SourceType)
  -> Map (Qualified Ident) (Tuple (Tuple SourceType NameKind) NameVisibility)
  -> m (Tuple (Tuple SourceAnn Ident) (Tuple Expr SourceType))
typeForBindingGroupElement (Tuple ident (Tuple val ty)) dict = do
  TypedValue' _ val' ty' <- bindNames dict $ infer val
  unifyTypes ty ty'
  pure (Tuple ident (Tuple (TypedValue true val' ty') ty'))

-- ─── instantiatePolyTypeWithUnknowns ─────────────────────────────────────────

instantiatePolyTypeWithUnknowns
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => Expr
  -> SourceType
  -> m (Tuple Expr SourceType)
instantiatePolyTypeWithUnknowns val (ForAll _ _ ident mbK ty _) = do
  u <- case mbK of
    Nothing -> internalCompilerError "instantiatePolyTypeWithUnknowns: Unelaborated forall"
    Just k  -> freshTypeWithKind k
  insertUnkName' u ident
  instantiatePolyTypeWithUnknowns val (replaceTypeVars ident u ty)
instantiatePolyTypeWithUnknowns val (ConstrainedType _ con ty) = do
  dicts <- getTypeClassDictionaries
  hints <- getHints
  instantiatePolyTypeWithUnknowns (App val (TypeClassDictionary con dicts hints)) ty
instantiatePolyTypeWithUnknowns val ty = pure (Tuple val ty)

instantiatePolyTypeWithUnknownsUntilVisible
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => Expr
  -> SourceType
  -> m (Tuple Expr SourceType)
instantiatePolyTypeWithUnknownsUntilVisible val (ForAll _ TypeVarInvisible ident mbK ty _) = do
  u <- case mbK of
    Nothing -> internalCompilerError "instantiatePolyTypeWithUnknownsUntilVisible: Unelaborated forall"
    Just k  -> freshTypeWithKind k
  insertUnkName' u ident
  instantiatePolyTypeWithUnknownsUntilVisible val (replaceTypeVars ident u ty)
instantiatePolyTypeWithUnknownsUntilVisible val ty = pure (Tuple val ty)

instantiateConstraint
  :: forall m
   . MonadState CheckState m
  => Expr
  -> SourceType
  -> m (Tuple Expr SourceType)
instantiateConstraint val (ConstrainedType _ con ty) = do
  dicts <- getTypeClassDictionaries
  hints <- getHints
  instantiateConstraint (App val (TypeClassDictionary con dicts hints)) ty
instantiateConstraint val ty = pure (Tuple val ty)

insertUnkName'
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => SourceType
  -> String
  -> m Unit
insertUnkName' (TUnknown _ i) n = insertUnkName i n
insertUnkName' _ _ = internalCompilerError "insertUnkName': type is not TUnknown"

-- ─── infer ───────────────────────────────────────────────────────────────────

infer
  :: forall m
   . MonadSupply m
  => MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => Expr
  -> m TypedValue'
infer val = withErrorMessageHint (ErrorInferringType val) (infer' val)

infer'
  :: forall m
   . MonadSupply m
  => MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => Expr
  -> m TypedValue'
infer' v@(Literal _ (NumericLiteral (Left _)))  = pure $ TypedValue' true v tyInt
infer' v@(Literal _ (NumericLiteral (Right _))) = pure $ TypedValue' true v tyNumber
infer' v@(Literal _ (StringLiteral _))          = pure $ TypedValue' true v tyString
infer' v@(Literal _ (CharLiteral _))            = pure $ TypedValue' true v tyChar
infer' v@(Literal _ (BooleanLiteral _))         = pure $ TypedValue' true v tyBoolean

infer' (Literal ss (ArrayLiteral vals)) = do
  ts  <- traverse infer vals
  els <- freshTypeWithKind kindType
  ts' <- for ts \(TypedValue' ch val t) -> do
    Tuple val' t' <- instantiatePolyTypeWithUnknowns val t
    unifyTypes els t'
    pure (TypedValue ch val' t')
  pure $ TypedValue' true (Literal ss (ArrayLiteral ts')) (srcTypeApp tyArray els)

infer' (Literal ss (ObjectLiteral ps)) = do
  ensureNoDuplicateProperties ps
  typedFields <- inferProperties ps
  let toRLI (Tuple l (Tuple _ t)) = srcRowListItem (Label l) t
      recordType  = srcTypeApp tyRecord $
        rowFromList (Tuple (map toRLI typedFields) (srcKindApp srcREmpty kindType))
      typedPs     = map (\(Tuple l (Tuple e t)) -> Tuple l (TypedValue true e t)) typedFields
  pure $ TypedValue' true (Literal ss (ObjectLiteral typedPs)) recordType

infer' (ObjectUpdate ob ps) = do
  ensureNoDuplicateProperties ps
  rowType <- freshTypeWithKind (kindRowOf kindType)
  obTypes <- traverse (\(Tuple l _) -> map (Tuple (Label l)) (freshTypeWithKind kindType)) ps
  let obItems      = map (\(Tuple l t) -> srcRowListItem l t) obTypes
      obRecordType = srcTypeApp tyRecord (rowFromList (Tuple obItems rowType))
  ob' <- (\e -> TypedValue true e obRecordType) <<< tvToExpr <$> check ob obRecordType
  typedFields <- inferProperties ps
  let newItems      = map (\(Tuple l (Tuple _ t)) -> srcRowListItem (Label l) t) typedFields
      ps'           = map (\(Tuple l (Tuple e t)) -> Tuple l (TypedValue true e t)) typedFields
      newRecordType = srcTypeApp tyRecord (rowFromList (Tuple newItems rowType))
  pure $ TypedValue' true (ObjectUpdate ob' ps') newRecordType

infer' (Accessor prop val) = withErrorMessageHint (ErrorCheckingAccessor val prop) do
  field <- freshTypeWithKind kindType
  rest  <- freshTypeWithKind (kindRowOf kindType)
  typed <- tvToExpr <$> check val (srcTypeApp tyRecord (srcRCons (Label prop) field rest))
  pure $ TypedValue' true (Accessor prop typed) field

infer' (Abs binder ret) = case binder of
  VarBinder ss arg -> do
    ty <- freshTypeWithKind kindType
    withBindingGroupVisible $
      bindLocalVariables [ Tuple (Tuple ss arg) (Tuple ty Defined) ] do
        body@(TypedValue' _ _ bodyTy) <- infer' ret
        Tuple body' bodyTy' <- instantiatePolyTypeWithUnknowns (tvToExpr body) bodyTy
        pure $ TypedValue' true (Abs (VarBinder ss arg) body') (function ty bodyTy')
  _ -> internalCompilerError "infer' Abs: binder was not desugared"

infer' (App f arg) = do
  f'@(TypedValue' _ _ ft) <- infer f
  Tuple ret app <- checkFunctionApplication (tvToExpr f') ft arg
  pure $ TypedValue' true app ret

infer' (VisibleTypeApp valFn (TypeWildcard _ _)) = do
  TypedValue' _ valFn' valTy <- infer valFn
  Tuple valFn'' valTy' <- instantiatePolyTypeWithUnknownsUntilVisible valFn' valTy
  case valTy' of
    ForAll qAnn _ qName qKind qBody qSko ->
      pure $ TypedValue' true valFn'' (ForAll qAnn TypeVarInvisible qName qKind qBody qSko)
    _ -> throwError $ errorMessage (CannotSkipTypeApplication valTy')

infer' (VisibleTypeApp valFn tyArg) = do
  TypedValue' _ valFn' valTy <- infer valFn
  tyArg' <- (introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards) tyArg
  Tuple valFn'' valTy' <- instantiatePolyTypeWithUnknownsUntilVisible valFn' valTy
  case valTy' of
    ForAll _ _ qName (Just qKind) qBody _ -> do
      tyArg'' <- (replaceAllTypeSynonyms <=< checkKind tyArg') qKind
      let resTy = replaceTypeVars qName tyArg'' qBody
      Tuple valFn''' resTy' <- instantiateConstraint valFn'' resTy
      pure $ TypedValue' true valFn''' resTy'
    _ -> throwError $ errorMessage (CannotApplyExpressionOfTypeOnType valTy tyArg)

infer' (Var ss var) = do
  checkVisibility var
  ty <- (introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards <=< lookupVariable) var
  case ty of
    ConstrainedType _ con ty' -> do
      dicts <- getTypeClassDictionaries
      hints <- getHints
      pure $ TypedValue' true (App (Var ss var) (TypeClassDictionary con dicts hints)) ty'
    _ -> pure $ TypedValue' true (Var ss var) ty

infer' v@(Constructor ss c@(Qualified qb name)) = do
  env <- getEnv
  let Environment e = env
  mResult <- case qb of
    BySourcePos _ -> do
      mn <- unsafeCheckCurrentModule
      pure (Map.lookup (Qualified (ByModuleName mn) name) e.dataConstructors)
    ByModuleName _ -> pure (Map.lookup c e.dataConstructors)
  case mResult of
    Nothing -> throwError $ errorMessage (UnknownName (map DctorName c))
    Just (Tuple (Tuple (Tuple _ _) ty) _) ->
      TypedValue' true v <$> (introduceSkolemScope <=< replaceAllTypeSynonyms) ty

infer' (Case vals binders) = do
  Tuple vals' ts <- instantiateForBinders vals binders
  ret <- freshTypeWithKind kindType
  binders' <- checkBinders ts ret binders
  pure $ TypedValue' true (Case vals' binders') ret

infer' (IfThenElse cond th el) = do
  cond' <- tvToExpr <$> check cond tyBoolean
  th'@(TypedValue' _ _ thTy) <- infer th
  el'@(TypedValue' _ _ elTy) <- infer el
  Tuple th'' thTy' <- instantiatePolyTypeWithUnknowns (tvToExpr th') thTy
  Tuple el'' elTy' <- instantiatePolyTypeWithUnknowns (tvToExpr el') elTy
  unifyTypes thTy' elTy'
  pure $ TypedValue' true (IfThenElse cond' th'' el'') thTy'

infer' (Let w ds val) = do
  Tuple ds' tv@(TypedValue' _ _ valTy) <- inferLetBinding [] ds val infer
  pure $ TypedValue' true (Let w ds' (tvToExpr tv)) valTy

infer' (DeferredDictionary className tys) = do
  dicts <- getTypeClassDictionaries
  hints <- getHints
  con <- checkConstraint (srcConstraint className [] tys Nothing)
  pure $ TypedValue' false
    (TypeClassDictionary con dicts hints)
    (Array.foldl srcTypeApp (srcTypeConstructor (map coerceProperName className)) tys)

infer' (TypedValue checkType val ty) = do
  moduleName <- unsafeCheckCurrentModule
  Tuple (Tuple args elabTy) _kind <- kindOfWithScopedVars ty
  checkTypeKind ty _kind
  ty' <- (introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards) elabTy
  tv <- if checkType
    then withScopedTypeVars moduleName args (check val ty')
    else pure (TypedValue' false val ty)
  pure $ TypedValue' true (tvToExpr tv) ty'

infer' (Hole name) = do
  ty  <- freshTypeWithKind kindType
  ctx <- getLocalContext
  tell $ errorMessage (HoleInferredType name ty ctx Nothing)
  pure $ TypedValue' true (Hole name) ty

infer' (PositionedValue pos c val) = warnAndRethrowWithPositionTC pos do
  TypedValue' t v ty <- infer' val
  pure $ TypedValue' t (PositionedValue pos c v) ty

infer' v = internalCompilerError $ "Invalid argument to infer: " <> show v

-- ─── inferProperties ─────────────────────────────────────────────────────────

inferProperties
  :: forall m
   . MonadSupply m
  => MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => Array (Tuple PSString Expr)
  -> m (Array (Tuple PSString (Tuple Expr SourceType)))
inferProperties = traverse (traverse inferWithinRecord)
  where
  inferWithinRecord e = do
    TypedValue' _ v t <- infer e
    if propertyShouldInstantiate e
      then instantiatePolyTypeWithUnknowns v t
      else pure (Tuple v t)

propertyShouldInstantiate :: Expr -> Boolean
propertyShouldInstantiate (Var _ _)               = true
propertyShouldInstantiate (Constructor _ _)       = true
propertyShouldInstantiate (VisibleTypeApp e _)    = propertyShouldInstantiate e
propertyShouldInstantiate (PositionedValue _ _ e) = propertyShouldInstantiate e
propertyShouldInstantiate _                       = false

-- ─── inferLetBinding ─────────────────────────────────────────────────────────

spanStart :: SourceSpan -> SourcePos
spanStart (SourceSpan s) = s.start

inferLetBinding
  :: forall m
   . MonadSupply m
  => MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => Array Declaration
  -> Array Declaration
  -> Expr
  -> (Expr -> m TypedValue')
  -> m (Tuple (Array Declaration) TypedValue')
inferLetBinding seen decls ret j = case Array.uncons decls of
  Nothing -> Tuple seen <$> withBindingGroupVisible (j ret)
  Just { head: ValueDeclaration (ValueDeclarationData vd), tail: rest }
    | Array.length vd.valdeclBinders == 0
    , [GuardedExpr [] (TypedValue checkType val ty)] <- vd.valdeclExpression -> do
        let Tuple ss _ = vd.valdeclSourceAnn
            ident      = vd.valdeclIdent
            nameKind   = vd.valdeclName
        moduleName <- unsafeCheckCurrentModule
        TypedValue' _ val' ty'' <- warnAndRethrowWithPositionTC ss do
          Tuple (Tuple args elabTy) _kind <- kindOfWithScopedVars ty
          checkTypeKind ty _kind
          let dict = Map.singleton (Qualified byNullSourcePos ident)
                       (Tuple (Tuple elabTy nameKind) Undefined)
          ty' <- (introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards) elabTy
          if checkType
            then withScopedTypeVars moduleName args (bindNames dict (check val ty'))
            else pure (TypedValue' checkType val elabTy)
        let newDecl = ValueDeclaration
              (ValueDeclarationData vd { valdeclExpression = [GuardedExpr [] (TypedValue checkType val' ty'')] })
        bindNames
          (Map.singleton (Qualified byNullSourcePos ident)
            (Tuple (Tuple ty'' nameKind) Defined))
          (inferLetBinding (seen <> [newDecl]) rest ret j)
  Just { head: ValueDeclaration (ValueDeclarationData vd), tail: rest }
    | Array.length vd.valdeclBinders == 0
    , [GuardedExpr [] val] <- vd.valdeclExpression -> do
        let Tuple ss _ = vd.valdeclSourceAnn
            ident      = vd.valdeclIdent
            nameKind   = vd.valdeclName
        valTy <- freshTypeWithKind kindType
        TypedValue' _ val' valTy' <- warnAndRethrowWithPositionTC ss do
          let dict = Map.singleton (Qualified byNullSourcePos ident)
                       (Tuple (Tuple valTy nameKind) Undefined)
          bindNames dict (infer val)
        warnAndRethrowWithPositionTC ss (unifyTypes valTy valTy')
        let newDecl = ValueDeclaration (ValueDeclarationData vd { valdeclExpression = [GuardedExpr [] val'] })
        bindNames
          (Map.singleton (Qualified byNullSourcePos ident)
            (Tuple (Tuple valTy' nameKind) Defined))
          (inferLetBinding (seen <> [newDecl]) rest ret j)
  Just { head: BindingGroupDeclaration ds, tail: rest } -> do
    moduleName <- unsafeCheckCurrentModule
    let dsArr = Array.fromFoldable (map (\(Tuple sai (Tuple _ v)) -> Tuple sai v) ds)
    SplitBindingGroup grp <- typeDictionaryForBindingGroup Nothing dsArr
    ds1' <- parU grp.splitTyped \e -> checkTypedBindingGroupElement moduleName e grp.splitDict
    ds2' <- traverse (\e -> typeForBindingGroupElement e grp.splitDict) grp.splitUntyped
    let allDs     = ds1' <> ds2'
        nelItems  = map (\(Tuple sai (Tuple val' _)) -> Tuple sai (Tuple Private val')) allDs
    case NEL.fromFoldable nelItems of
      Nothing  -> internalCompilerError "inferLetBinding: empty binding group"
      Just nel ->
        bindNames grp.splitDict do
          makeBindingGroupVisible
          inferLetBinding (seen <> [BindingGroupDeclaration nel]) rest ret j
  _ -> internalCompilerError "Invalid argument to inferLetBinding"

-- ─── inferBinder ─────────────────────────────────────────────────────────────

inferBinder
  :: forall m
   . MonadSupply m
  => MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => SourceType
  -> Binder
  -> m (Map Ident (Tuple SourceSpan SourceType))
inferBinder _ NullBinder = pure Map.empty
inferBinder val (LiteralBinder _ (StringLiteral _)) = unifyTypes val tyString *> pure Map.empty
inferBinder val (LiteralBinder _ (CharLiteral _)) = unifyTypes val tyChar *> pure Map.empty
inferBinder val (LiteralBinder _ (NumericLiteral (Left _))) = unifyTypes val tyInt *> pure Map.empty
inferBinder val (LiteralBinder _ (NumericLiteral (Right _))) = unifyTypes val tyNumber *> pure Map.empty
inferBinder val (LiteralBinder _ (BooleanLiteral _)) = unifyTypes val tyBoolean *> pure Map.empty
inferBinder val (VarBinder ss name) = pure (Map.singleton name (Tuple ss val))

inferBinder val (ConstructorBinder ss ctor@(Qualified qb ctorName) binders) = do
  env <- getEnv
  let Environment e = env
  mResult <- case qb of
    BySourcePos _ -> do
      mn <- unsafeCheckCurrentModule
      pure (Map.lookup (Qualified (ByModuleName mn) ctorName) e.dataConstructors)
    ByModuleName _ -> pure (Map.lookup ctor e.dataConstructors)
  case mResult of
    Just (Tuple (Tuple (Tuple _ _) ty) _) -> do
      Tuple _ fn <- instantiatePolyTypeWithUnknowns (Hole "data ctor dummy") ty
      fn' <- (introduceSkolemScope <=< replaceAllTypeSynonyms) fn
      let Tuple args ret = peelArgs fn'
          expected = Array.length args
          actual   = Array.length binders
      unless (expected == actual) $
        throwError $ errorMessage' ss (IncorrectConstructorArity ctor expected actual)
      unifyTypes ret val
      Map.unions <$> Array.zipWithA inferBinder (Array.reverse args) binders
    _ -> throwError $ errorMessage' ss (UnknownName (map DctorName ctor))
  where
  peelArgs :: SourceType -> Tuple (Array SourceType) SourceType
  peelArgs = go []
    where
    go args (TypeApp _ (TypeApp _ fn arg) ret')
      | fn == tyFunction = go (Array.cons arg args) ret'
    go args ret' = Tuple args ret'

inferBinder val (LiteralBinder _ (ObjectLiteral props)) = do
  row  <- freshTypeWithKind (kindRowOf kindType)
  rest <- freshTypeWithKind (kindRowOf kindType)
  m1   <- inferRowProperties row rest props
  unifyTypes val (srcTypeApp tyRecord row)
  pure m1
  where
  inferRowProperties nrow rw props = case Array.uncons props of
    Nothing -> unifyTypes nrow rw *> pure Map.empty
    Just { head: Tuple name binder, tail: binders' } -> do
      propTy <- freshTypeWithKind kindType
      m1 <- inferBinder propTy binder
      m2 <- inferRowProperties nrow (srcRCons (Label name) propTy rw) binders'
      pure (Map.union m1 m2)

inferBinder val (LiteralBinder _ (ArrayLiteral binders)) = do
  el <- freshTypeWithKind kindType
  m1 <- Map.unions <$> traverse (inferBinder el) binders
  unifyTypes val (srcTypeApp tyArray el)
  pure m1

inferBinder val (NamedBinder ss name binder) =
  warnAndRethrowWithPositionTC ss do
    m <- inferBinder val binder
    pure $ Map.insert name (Tuple ss val) m

inferBinder val (PositionedBinder pos _ binder) =
  warnAndRethrowWithPositionTC pos (inferBinder val binder)

inferBinder val (TypedBinder ty binder) = do
  Tuple elabTy kind <- kindOf ty
  checkTypeKind ty kind
  ty1 <- (introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards) elabTy
  unifyTypes val ty1
  inferBinder ty1 binder

inferBinder _ (OpBinder _ _) =
  internalCompilerError "OpBinder should have been desugared"
inferBinder _ (BinaryNoParensBinder _ _ _) =
  internalCompilerError "BinaryNoParensBinder should have been desugared"
inferBinder _ (ParensInBinder _) =
  internalCompilerError "ParensInBinder should have been desugared"

-- ─── binderRequiresMonotype ──────────────────────────────────────────────────

binderRequiresMonotype :: Binder -> Boolean
binderRequiresMonotype NullBinder               = false
binderRequiresMonotype (VarBinder _ _)          = false
binderRequiresMonotype (NamedBinder _ _ b)      = binderRequiresMonotype b
binderRequiresMonotype (PositionedBinder _ _ b) = binderRequiresMonotype b
binderRequiresMonotype (TypedBinder ty b)       = isMonoType ty || binderRequiresMonotype b
binderRequiresMonotype _                        = true

-- ─── instantiateForBinders ───────────────────────────────────────────────────

instantiateForBinders
  :: forall m
   . MonadSupply m
  => MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => Array Expr
  -> Array CaseAlternative
  -> m (Tuple (Array Expr) (Array SourceType))
instantiateForBinders vals cas = unzipPairs <$>
  Array.zipWithA (\val inst -> do
    TypedValue' _ val' ty <- infer val
    if inst
      then instantiatePolyTypeWithUnknowns val' ty
      else pure (Tuple val' ty)
  ) vals shouldInstantiate
  where
  shouldInstantiate :: Array Boolean
  shouldInstantiate =
    map (Array.any binderRequiresMonotype) $
    Array.transpose $
    map (\(CaseAlternative ca) -> ca.caseAlternativeBinders) cas

  unzipPairs :: Array (Tuple Expr SourceType) -> Tuple (Array Expr) (Array SourceType)
  unzipPairs ts = Tuple (map fst ts) (map snd ts)

-- ─── checkBinders ────────────────────────────────────────────────────────────

checkBinders
  :: forall m
   . MonadSupply m
  => MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => Array SourceType
  -> SourceType
  -> Array CaseAlternative
  -> m (Array CaseAlternative)
checkBinders nvals ret alts = case Array.uncons alts of
  Nothing -> pure []
  Just { head: CaseAlternative ca, tail: bs } -> do
    let binders = ca.caseAlternativeBinders
        result  = ca.caseAlternativeResult
    guardWith (errorMessage (OverlappingArgNames Nothing)) $
      let ns = Array.concatMap binderNames binders
      in Array.length (Array.nub ns) == Array.length ns
    m1 <- Map.unions <$> Array.zipWithA inferBinder nvals binders
    r  <- bindLocalVariables
            (map (\(Tuple name (Tuple ss ty)) -> Tuple (Tuple ss name) (Tuple ty Defined))
                 (Map.toUnfoldable m1 :: Array _))
          $ CaseAlternative <<< { caseAlternativeBinders: binders, caseAlternativeResult: _ }
              <$> traverse (\ge -> checkGuardedRhs ge ret) result
    rs <- checkBinders nvals ret bs
    pure (Array.cons r rs)

checkGuardedRhs
  :: forall m
   . MonadSupply m
  => MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => GuardedExpr
  -> SourceType
  -> m GuardedExpr
checkGuardedRhs (GuardedExpr gs rhs) ret = case Array.uncons gs of
  Nothing -> do
    rhs' <- TypedValue true <$> (tvToExpr <$> check rhs ret) <*> pure ret
    pure $ GuardedExpr [] rhs'
  Just { head: ConditionGuard cond, tail: guards } -> do
    cond' <- withErrorMessageHint ErrorCheckingGuard (check cond tyBoolean)
    GuardedExpr guards' rhs' <- checkGuardedRhs (GuardedExpr guards rhs) ret
    pure $ GuardedExpr (Array.cons (ConditionGuard (tvToExpr cond')) guards') rhs'
  Just { head: PatternGuard binder expr, tail: guards } -> do
    tv@(TypedValue' _ _ ty) <- infer expr
    variables <- inferBinder ty binder
    GuardedExpr guards' rhs' <-
      bindLocalVariables
        (map (\(Tuple name (Tuple ss bty)) -> Tuple (Tuple ss name) (Tuple bty Defined))
             (Map.toUnfoldable variables :: Array _))
        (checkGuardedRhs (GuardedExpr guards rhs) ret)
    pure $ GuardedExpr (Array.cons (PatternGuard binder (tvToExpr tv)) guards') rhs'

-- ─── check ───────────────────────────────────────────────────────────────────

check
  :: forall m
   . MonadSupply m
  => MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => Expr
  -> SourceType
  -> m TypedValue'
check val ty = withErrorMessageHint' val (ErrorCheckingType val ty) (check' val ty)

check'
  :: forall m
   . MonadSupply m
  => MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => Expr
  -> SourceType
  -> m TypedValue'
check' val (ForAll ann vis ident mbK ty _) = do
  env   <- getEnv
  mn    <- gets \(CheckState s) -> s.checkCurrentModule
  scope <- newSkolemScope
  sko   <- newSkolemConstant
  let ssAnn = case val of
                PositionedValue pos _ _ -> Tuple pos []
                _                      -> nullSourceAnn
      sk    = skolemize ssAnn ident mbK sko scope ty
      Environment e = env
      skVal = case Map.lookup (Qualified (byMaybeModuleName mn) (ProperName ident)) e.types of
                Just _  -> skolemizeTypesInValue ssAnn ident mbK sko scope val
                Nothing -> val
  val' <- tvToExpr <$> check skVal sk
  pure $ TypedValue' true val' (ForAll ann vis ident mbK ty (Just scope))

check' val t@(ConstrainedType _ con@(Constraint c) ty) = do
  TypeClassData tcData <- lookupTypeClass c.constraintClass
  let Qualified _ (ProperName className) = c.constraintClass
  dictName <- if tcData.typeClassIsEmpty
    then pure UnusedIdent
    else freshIdent ("dict" <> className)
  dicts <- newDictionaries [] (Qualified byNullSourcePos dictName) con
  val'  <- withBindingGroupVisible $ withTypeClassDictionaries dicts $ check val ty
  pure $ TypedValue' true (Abs (VarBinder nullSourceSpan dictName) (tvToExpr val')) t

check' val u@(TUnknown _ _) = do
  val'@(TypedValue' _ _ ty) <- infer val
  Tuple val'' ty' <- instantiatePolyTypeWithUnknowns (tvToExpr val') ty
  unifyTypes ty' u
  pure $ TypedValue' true val'' ty'

check' v@(Literal _ (NumericLiteral (Left _)))  t | t == tyInt     = pure $ TypedValue' true v t
check' v@(Literal _ (NumericLiteral (Right _))) t | t == tyNumber  = pure $ TypedValue' true v t
check' v@(Literal _ (StringLiteral _))          t | t == tyString  = pure $ TypedValue' true v t
check' v@(Literal _ (CharLiteral _))            t | t == tyChar    = pure $ TypedValue' true v t
check' v@(Literal _ (BooleanLiteral _))         t | t == tyBoolean = pure $ TypedValue' true v t

check' (Literal ss (ArrayLiteral vals)) t@(TypeApp _ a ty) = do
  unifyTypes a tyArray
  array <- Literal ss <<< ArrayLiteral <<< map tvToExpr <$> traverse (\v -> check v ty) vals
  pure $ TypedValue' true array t

check' (Abs binder ret) ty@(TypeApp _ (TypeApp _ t argTy) retTy) = case binder of
  VarBinder ss arg -> do
    unifyTypes t tyFunction
    ret' <- withBindingGroupVisible $
      bindLocalVariables [ Tuple (Tuple ss arg) (Tuple argTy Defined) ] $
        check ret retTy
    pure $ TypedValue' true (Abs (VarBinder ss arg) (tvToExpr ret')) ty
  _ -> internalCompilerError "check' Abs: binder was not desugared"

check' (App f arg) ret = do
  f'@(TypedValue' _ _ ft) <- infer f
  Tuple retTy app <- checkFunctionApplication (tvToExpr f') ft arg
  elaborate <- subsumes retTy ret
  pure $ TypedValue' true (elaborate app) ret

check' v@(Var _ var) ty = do
  checkVisibility var
  repl <- (introduceSkolemScope <=< replaceAllTypeSynonyms <=< lookupVariable) var
  ty'  <- (introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards) ty
  elaborate <- subsumes repl ty'
  pure $ TypedValue' true (elaborate v) ty'

check' (DeferredDictionary className tys) ty = do
  dicts <- getTypeClassDictionaries
  hints <- getHints
  con <- checkConstraint (srcConstraint className [] tys Nothing)
  pure $ TypedValue' false (TypeClassDictionary con dicts hints) ty

check' (TypedValue checkType val ty1) ty2 = do
  moduleName <- unsafeCheckCurrentModule
  Tuple (Tuple args elabTy1) kind1 <- kindOfWithScopedVars ty1
  Tuple elabTy2 kind2 <- kindOf ty2
  unifyKinds' kind1 kind2
  checkTypeKind ty1 kind1
  ty1' <- (introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards) elabTy1
  ty2' <- (introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards) elabTy2
  elaborate <- subsumes ty1' ty2'
  val' <- if checkType
    then withScopedTypeVars moduleName args (tvToExpr <$> check val ty1')
    else pure val
  pure $ TypedValue' true (TypedValue checkType (elaborate val') ty1') ty2'

check' (Case vals binders) ret = do
  Tuple vals' ts <- instantiateForBinders vals binders
  binders' <- checkBinders ts ret binders
  pure $ TypedValue' true (Case vals' binders') ret

check' (IfThenElse cond th el) ty = do
  cond' <- tvToExpr <$> check cond tyBoolean
  th'   <- tvToExpr <$> check th ty
  el'   <- tvToExpr <$> check el ty
  pure $ TypedValue' true (IfThenElse cond' th' el') ty

check' e@(Literal ss (ObjectLiteral ps)) t@(TypeApp _ obj row) | obj == tyRecord = do
  ensureNoDuplicateProperties ps
  ps' <- checkProperties e ps row false
  pure $ TypedValue' true (Literal ss (ObjectLiteral ps')) t

check' (DerivedInstancePlaceholder name strategy) t = do
  d  <- deriveInstance t name strategy
  d' <- tvToExpr <$> check' d t
  pure $ TypedValue' true d' t

check' e@(ObjectUpdate obj ps) t@(TypeApp _ o row) | o == tyRecord = do
  ensureNoDuplicateProperties ps
  let Tuple propsToCheck rest = rowToList row
      updateLabels = map (\(Tuple l _) -> Label l) ps
      { yes: removedProps, no: remainingProps } =
        Array.partition (\(RowListItem ri) -> Array.elem ri.rowListLabel updateLabels) propsToCheck
  us <- traverse (\(RowListItem ri) ->
          map (srcRowListItem ri.rowListLabel) (freshTypeWithKind kindType)) removedProps
  obj' <- tvToExpr <$> check obj
    (srcTypeApp tyRecord (rowFromList (Tuple (us <> remainingProps) rest)))
  ps' <- checkProperties e ps row true
  pure $ TypedValue' true (ObjectUpdate obj' ps') t

check' (Accessor prop val) ty = withErrorMessageHint (ErrorCheckingAccessor val prop) do
  rest <- freshTypeWithKind (kindRowOf kindType)
  val' <- tvToExpr <$> check val (srcTypeApp tyRecord (srcRCons (Label prop) ty rest))
  pure $ TypedValue' true (Accessor prop val') ty

check' v@(Constructor _ c) ty = do
  env <- getEnv
  let Environment e = env
  case Map.lookup c e.dataConstructors of
    Nothing -> throwError $ errorMessage (UnknownName (map DctorName c))
    Just (Tuple (Tuple (Tuple _ _) ty1) _) -> do
      repl      <- (introduceSkolemScope <=< replaceAllTypeSynonyms) ty1
      ty'       <- (introduceSkolemScope <=< replaceAllTypeSynonyms) ty
      elaborate <- subsumes repl ty'
      pure $ TypedValue' true (elaborate v) ty'

check' (Let w ds val) ty = do
  Tuple ds' val' <- inferLetBinding [] ds val (\v -> check v ty)
  pure $ TypedValue' true (Let w ds' (tvToExpr val')) ty

check' val (KindedType _ ty _kind) = do
  checkTypeKind ty _kind
  val' <- tvToExpr <$> check' val ty
  pure $ TypedValue' true val' (KindedType nullSourceAnn ty _kind)

check' (PositionedValue pos c val) ty = warnAndRethrowWithPositionTC pos do
  TypedValue' t v ty' <- check' val ty
  pure $ TypedValue' t (PositionedValue pos c v) ty'

check' val ty = do
  TypedValue' _ val' ty' <- infer val
  elaborate <- subsumes ty' ty
  pure $ TypedValue' true (elaborate val') ty

-- ─── checkProperties ─────────────────────────────────────────────────────────

checkProperties
  :: forall m
   . MonadSupply m
  => MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => Expr
  -> Array (Tuple PSString Expr)
  -> SourceType
  -> Boolean
  -> m (Array (Tuple PSString Expr))
checkProperties expr ps row lax = do
  let Tuple ts' r' = rowToList row
      tsList = map (\(RowListItem ri) -> Tuple ri.rowListLabel ri.rowListType) ts'
  result <- go ps tsList r'
  pure (map (map tvToExpr) result)
  where
  go :: Array (Tuple PSString Expr) -> Array (Tuple Label SourceType) -> SourceType
     -> m (Array (Tuple PSString TypedValue'))
  go ps ts r = case Array.uncons ps of
    Nothing -> case Array.uncons ts of
      Nothing -> case r of
        REmpty _ -> pure []
        TUnknown _ _ | lax -> pure []
        TUnknown _ _ -> unifyTypes r srcREmpty *> pure []
        Skolem _ _ _ _ _ | lax -> pure []
        _ -> throwError (errorMessage (ExprDoesNotHaveType expr (srcTypeApp tyRecord row)))
      Just { head: Tuple p _, tail: _ } ->
        if lax then pure []
        else throwError (errorMessage (PropertyIsMissing p))
    Just { head: Tuple p v, tail: ps' } -> case r of
      REmpty _ | Array.null ts ->
        throwError (errorMessage (AdditionalProperty (Label p)))
      _ -> case Array.find (\(Tuple l _) -> l == Label p) ts of
        Nothing -> do
          Tuple v' ty <- inferWithinRecord v
          rest <- freshTypeWithKind (kindRowOf kindType)
          unifyTypes r (srcRCons (Label p) ty rest)
          ps'' <- go ps' ts rest
          pure $ Array.cons (Tuple p (TypedValue' true v' ty)) ps''
        Just (Tuple _ ty) -> do
          v' <- check v ty
          let ts' = deleteFirst (\(Tuple l t) -> l == Label p && t == ty) ts
          ps'' <- go ps' ts' r
          pure $ Array.cons (Tuple p v') ps''

  inferWithinRecord :: Expr -> m (Tuple Expr SourceType)
  inferWithinRecord e = do
    TypedValue' _ v t <- infer e
    if propertyShouldInstantiate e
      then instantiatePolyTypeWithUnknowns v t
      else pure (Tuple v t)

  deleteFirst :: forall a. (a -> Boolean) -> Array a -> Array a
  deleteFirst p arr = case Array.findIndex p arr of
    Nothing -> arr
    Just i  -> fromMaybe arr (Array.deleteAt i arr)

-- ─── checkFunctionApplication ────────────────────────────────────────────────

checkFunctionApplication
  :: forall m
   . MonadSupply m
  => MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => Expr
  -> SourceType
  -> Expr
  -> m (Tuple SourceType Expr)
checkFunctionApplication fn fnTy arg =
  withErrorMessageHint' fn (ErrorInApplication fn fnTy arg) do
    subst <- gets \(CheckState s) -> s.checkSubstitution
    checkFunctionApplication' fn (substituteType subst fnTy) arg

checkFunctionApplication'
  :: forall m
   . MonadSupply m
  => MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => Expr
  -> SourceType
  -> Expr
  -> m (Tuple SourceType Expr)
checkFunctionApplication' fn (TypeApp _ (TypeApp _ tyFn argTy) retTy) arg = do
  unifyTypes tyFn tyFunction
  arg' <- tvToExpr <$> check arg argTy
  pure (Tuple retTy (App fn arg'))
checkFunctionApplication' fn (ForAll _ _ ident mbK ty _) arg = do
  u <- case mbK of
    Nothing -> internalCompilerError "checkFunctionApplication': Unelaborated forall"
    Just k  -> freshTypeWithKind k
  insertUnkName' u ident
  checkFunctionApplication fn (replaceTypeVars ident u ty) arg
checkFunctionApplication' fn (KindedType _ ty _) arg =
  checkFunctionApplication fn ty arg
checkFunctionApplication' fn (ConstrainedType _ con fnTy) arg = do
  dicts <- getTypeClassDictionaries
  hints <- getHints
  checkFunctionApplication' (App fn (TypeClassDictionary con dicts hints)) fnTy arg
checkFunctionApplication' fn fnTy arg@(TypeClassDictionary _ _ _) =
  pure (Tuple fnTy (App fn arg))
checkFunctionApplication' fn u arg = do
  TypedValue' _ arg' t <- infer arg
  Tuple arg'' t' <- instantiatePolyTypeWithUnknowns arg' t
  ret <- freshTypeWithKind kindType
  unifyTypes u (function t' ret)
  pure (Tuple ret (App fn arg''))

-- ─── ensureNoDuplicateProperties ─────────────────────────────────────────────

ensureNoDuplicateProperties
  :: forall m
   . MonadError MultipleErrors m
  => Array (Tuple PSString Expr)
  -> m Unit
ensureNoDuplicateProperties ps =
  let ls = map fst ps
  in if Array.length (Array.nub ls) == Array.length ls
     then pure unit
     else case Array.findMap (\l ->
               if Array.length (Array.filter (_ == l) ls) > 1 then Just l else Nothing) ls of
            Just l  -> throwError (errorMessage (DuplicateLabel (Label l) Nothing))
            Nothing -> pure unit

-- ─── isInternal / withErrorMessageHint' ──────────────────────────────────────

isInternal :: Expr -> Boolean
isInternal (PositionedValue _ _ v)            = isInternal v
isInternal (TypedValue _ v _)                 = isInternal v
isInternal (Constructor _ (Qualified _ name)) = isDictTypeName name
isInternal (DerivedInstancePlaceholder _ _)   = true
isInternal _                                  = false

withErrorMessageHint'
  :: forall m a
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => Expr
  -> ErrorMessageHint
  -> m a
  -> m a
withErrorMessageHint' expr h action =
  if isInternal expr then action else withErrorMessageHint h action
