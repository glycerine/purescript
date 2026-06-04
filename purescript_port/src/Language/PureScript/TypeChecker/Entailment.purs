-- | Type class entailment solver.
-- | Port of Language.PureScript.TypeChecker.Entailment from Haskell.
module Language.PureScript.TypeChecker.Entailment
  ( Evidence(..)
  , Reflectable(..)
  , InstanceContext
  , SolverOptions
  , replaceTypeClassDictionaries
  , newDictionaries
  , entails
  , findDicts
  ) where

import Prelude

import Control.Monad (class Monad)
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.State (StateT(..), evalStateT, execStateT, get, gets, modify_, runStateT)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Supply.Class (class MonadSupply, freshIdent)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Class (class MonadWriter, tell)
import Control.Monad.Writer.Trans (WriterT(..), runWriterT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl, foldr, foldM, for_, traverse_)
import Data.Monoid.Disj (Disj(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String (stripPrefix, stripSuffix, Pattern(..))
import Data.Traversable (traverse, for)
import Data.Tuple (Tuple(..), fst, snd)

import Unsafe.Coerce as Unsafe.Coerce

import Language.PureScript.AST.Binders (Binder(..))
import Language.PureScript.AST.Declarations
  ( ErrorMessageHint(..)
  , Expr(..)
  , UnknownsHint(..)
  )
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (SourceAnn, nullSourceAnn, nullSourceSpan)
import Language.PureScript.AST.Traversals (everywhereOnValuesTopDownM)
import Language.PureScript.Constants.Prim as C
import Language.PureScript.Environment
  ( Environment(..)
  , FunctionalDependency(..)
  , TypeClassData(..)
  , dictTypeName
  )
import Language.PureScript.Errors
  ( MultipleErrors
  , SimpleErrorMessage(..)
  , addHint
  , addHints
  , errorMessage
  , internalCompilerError
  , rethrow
  )
import Language.PureScript.Label (Label(..), runLabel)
import Language.PureScript.Names
  ( ClassName
  , Ident(..)
  , ModuleName(..)
  , ProperName(..)
  , Qualified(..)
  , QualifiedBy(..)
  , byMaybeModuleName
  , byNullSourcePos
  , coerceProperName
  , disqualify
  , getQual
  , runProperName
  )
import Language.PureScript.PSString (PSString, mkString, decodeString)
import Language.PureScript.TypeChecker.Entailment.Coercible
  ( GivenSolverState(..)
  , WantedSolverState(..)
  , initialGivenSolverState
  , initialWantedSolverState
  , insoluble
  , solveGivens
  , solveWanteds
  )
import Language.PureScript.TypeChecker.Entailment.IntCompare (mkFacts, mkRelation, solveRelation)
import Language.PureScript.TypeChecker.Kinds (elaborateKind, unifyKinds')
import Language.PureScript.TypeChecker.Monad
  ( CheckState(..)
  , Substitution(..)
  , withErrorMessageHint
  )
import Language.PureScript.TypeChecker.Synonyms (replaceAllTypeSynonyms)
import Language.PureScript.TypeChecker.Unify (freshTypeWithKind, substituteType, unifyTypes)
import Language.PureScript.TypeClassDictionaries (NamedDict, TypeClassDictionaryInScope(..), superclassName)
import Language.PureScript.Types
  ( Constraint(..)
  , RowListItem(..)
  , SourceConstraint
  , SourceType
  , Type(..)
  , alignRowsWith
  , everythingOnTypes
  , isREmpty
  , isREmptyKinded
  , mapConstraintArgsAll
  , overConstraintArgsAll
  , replaceAllTypeVars
  , rowFromList
  , rowToList
  , rowToSortedList
  , srcConstraint
  , srcKindApp
  , srcRCons
  , srcREmpty
  , srcTypeApp
  , srcTypeConstructor
  , srcTypeLevelInt
  , srcTypeLevelString
  , srcTypeVar
  )

-- | Describes what sort of dictionary to generate for type class instances
data Evidence
  -- | An existing named instance
  = NamedInstance (Qualified Ident)
  -- | Warn type class with a user-defined warning message
  | WarnInstance SourceType
  -- | The IsSymbol type class for a given Symbol literal
  | IsSymbolInstance PSString
  -- | The Reflectable type class for a reflectable kind
  | ReflectableInstance Reflectable
  -- | For any solved type class with no members
  | EmptyClassInstance

derive instance eqEvidence :: Eq Evidence

-- | Describes kinds that are reflectable to the term-level
data Reflectable
  = ReflectableInt Int
  | ReflectableString PSString
  | ReflectableBoolean Boolean
  | ReflectableOrdering Ordering

derive instance eqReflectable :: Eq Reflectable

-- | Reflect a reflectable type into an expression
asExpression :: Reflectable -> Expr
asExpression = case _ of
  ReflectableInt n    -> Literal nullSourceSpan (NumericLiteral (Left n))
  ReflectableString s -> Literal nullSourceSpan (StringLiteral s)
  ReflectableBoolean b -> Literal nullSourceSpan (BooleanLiteral b)
  ReflectableOrdering o -> Constructor nullSourceSpan $ case o of
    LT -> Qualified (ByModuleName (ModuleName "Prim.Ordering")) (ProperName "LT")
    EQ -> Qualified (ByModuleName (ModuleName "Prim.Ordering")) (ProperName "EQ")
    GT -> Qualified (ByModuleName (ModuleName "Prim.Ordering")) (ProperName "GT")

-- | Extract the identifier of a named instance
namedInstanceIdentifier :: Evidence -> Maybe (Qualified Ident)
namedInstanceIdentifier (NamedInstance i) = Just i
namedInstanceIdentifier _ = Nothing

-- | Description of a type class dictionary with instance evidence
type TypeClassDict = TypeClassDictionaryInScope Evidence

-- | The 'InstanceContext' tracks those constraints which can be satisfied.
type InstanceContext =
  Map QualifiedBy
    (Map (Qualified (ProperName ClassName))
      (Map (Qualified Ident) (NonEmptyList NamedDict)))

-- | Find dictionaries for a given class and qualifier
findDicts
  :: InstanceContext
  -> Qualified (ProperName ClassName)
  -> QualifiedBy
  -> Array TypeClassDict
findDicts ctx cn qb =
  case Map.lookup qb ctx of
    Nothing -> []
    Just byClass ->
      case Map.lookup cn byClass of
        Nothing -> []
        Just byIdent ->
          Array.concatMap (\nel -> map (map NamedInstance) (Array.fromFoldable nel))
            (Array.fromFoldable (Map.values byIdent))

-- | A type substitution which makes an instance head match a list of types.
type Matching a = Map String a

combineContexts :: InstanceContext -> InstanceContext -> InstanceContext
combineContexts = Map.unionWith (Map.unionWith (Map.unionWith (<>)))

-- | Options for the constraint solver
type SolverOptions =
  { solverShouldGeneralize :: Boolean
  , solverDeferErrors      :: Boolean
  }

-- | Three options for handling a constraint
data EntailsResult a
  = Solved a TypeClassDict
  | Unsolved SourceConstraint
  | Deferred

data Matched t
  = Match t
  | Apart
  | Unknown

derive instance eqMatched :: Eq t => Eq (Matched t)
derive instance functorMatched :: Functor Matched

instance semigroupMatched :: Semigroup t => Semigroup (Matched t) where
  append (Match l) (Match r) = Match (l <> r)
  append Apart _    = Apart
  append _    Apart = Apart
  append _    _     = Unknown

instance monoidMatched :: Monoid t => Monoid (Matched t) where
  mempty = Match mempty

-- | Replace type class dictionary placeholders with inferred type class dictionaries
replaceTypeClassDictionaries
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => MonadSupply m
  => Boolean
  -> Expr
  -> m (Tuple Expr (Array (Tuple Ident (Tuple InstanceContext SourceConstraint))))
replaceTypeClassDictionaries shouldGeneralize expr =
  evalStateT (loop expr >>= generalizePass) Map.empty
  where
    -- Loop solving constraints until no more progress
    loop :: Expr -> StateT InstanceContext m (Tuple Expr Boolean)
    loop e = do
      Tuple e' (Tuple (Disj solved) _) <- runWriterT (deferPassExpr e)
      if solved
        then map (\(Tuple e'' _) -> Tuple e'' true) (loop e')
        else pure (Tuple e' false)

    -- This pass solves constraints where possible, deferring if not
    deferPassExpr
      :: Expr
      -> WriterT (Tuple (Disj Boolean) (Array (Tuple Ident (Tuple InstanceContext SourceConstraint)))) (StateT InstanceContext m) Expr
    deferPassExpr e = do
      let traversal = everywhereOnValuesTopDownM pure (goExpr true) pure
      traversal.expr e

    -- This pass generalizes any remaining constraints
    generalizePass
      :: Tuple Expr Boolean
      -> StateT InstanceContext m (Tuple Expr (Array (Tuple Ident (Tuple InstanceContext SourceConstraint))))
    generalizePass (Tuple e _) = do
      Tuple e' (Tuple _ unsolved) <- runWriterT (generalizeExpr e)
      pure (Tuple e' unsolved)

    generalizeExpr
      :: Expr
      -> WriterT (Tuple (Disj Boolean) (Array (Tuple Ident (Tuple InstanceContext SourceConstraint)))) (StateT InstanceContext m) Expr
    generalizeExpr e = do
      let traversal = everywhereOnValuesTopDownM pure (goExpr false) pure
      traversal.expr e

    goExpr
      :: Boolean
      -> Expr
      -> WriterT (Tuple (Disj Boolean) (Array (Tuple Ident (Tuple InstanceContext SourceConstraint)))) (StateT InstanceContext m) Expr
    goExpr deferErrors (TypeClassDictionary constraint context hints) =
      rethrow (addHints hints) (entailsInner shouldGeneralize deferErrors constraint context hints)
    goExpr _ other = pure other

-- | Internal entails called from replaceTypeClassDictionaries
entailsInner
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => MonadSupply m
  => Boolean
  -- ^ shouldGeneralize
  -> Boolean
  -- ^ deferErrors
  -> SourceConstraint
  -> Map QualifiedBy (Map (Qualified (ProperName ClassName)) (Map (Qualified Ident) (Array NamedDict)))
  -> Array ErrorMessageHint
  -> WriterT (Tuple (Disj Boolean) (Array (Tuple Ident (Tuple InstanceContext SourceConstraint)))) (StateT InstanceContext m) Expr
entailsInner shouldGeneralize deferErrors constraint context hints = do
  c' <- (lift <<< lift) (overConstraintArgsAll (traverse replaceAllTypeSynonyms) constraint)
  solveConstraint c'
  where
  opts :: SolverOptions
  opts = { solverShouldGeneralize: shouldGeneralize, solverDeferErrors: deferErrors }

  -- Convert Array NamedDict map to InstanceContext (NonEmptyList NamedDict map)
  arrayCtxToInstanceCtx
    :: Map QualifiedBy (Map (Qualified (ProperName ClassName)) (Map (Qualified Ident) (Array NamedDict)))
    -> InstanceContext
  arrayCtxToInstanceCtx = Map.mapMaybe (\m1 ->
    let m2 = Map.mapMaybe (\m3 ->
                  let m4 = Map.mapMaybe NEL.fromFoldable m3
                  in if Map.isEmpty m4 then Nothing else Just m4
               ) m1
    in if Map.isEmpty m2 then Nothing else Just m2
    )

  solveConstraint
    :: SourceConstraint
    -> WriterT (Tuple (Disj Boolean) (Array (Tuple Ident (Tuple InstanceContext SourceConstraint)))) (StateT InstanceContext m) Expr
  solveConstraint = go 0 hints
    where
    go
      :: Int
      -> Array ErrorMessageHint
      -> SourceConstraint
      -> WriterT (Tuple (Disj Boolean) (Array (Tuple Ident (Tuple InstanceContext SourceConstraint)))) (StateT InstanceContext m) Expr
    go work _ (Constraint c) | work > 1000 =
      throwError (errorMessage (PossiblyInfiniteInstance c.constraintClass c.constraintArgs))
    go work hints' con@(Constraint c) = WriterT $ StateT $ \instanceCtx ->
      withErrorMessageHint (ErrorSolvingConstraint con)
        (runStateT (runWriterT goInner) instanceCtx)
      where
      goInner :: WriterT (Tuple (Disj Boolean) (Array (Tuple Ident (Tuple InstanceContext SourceConstraint)))) (StateT InstanceContext m) Expr
      goInner = do
        let className' = c.constraintClass
            conInfo    = c.constraintData

        -- Apply latest substitution (lift . lift to reach CheckState through WriterT then StateT InstanceContext)
        latestSubst <- lift $ lift $ gets \(CheckState s) -> s.checkSubstitution
        let kinds'' = map (substituteType latestSubst) c.constraintKindArgs
            tys''   = map (substituteType latestSubst) c.constraintArgs

        -- Get the environment from CheckState
        env <- lift $ lift $ gets \(CheckState s) -> s.checkEnv
        let Environment envR = env
        let classesInScope = envR.typeClasses

        TypeClassData tcData <- case Map.lookup className' classesInScope of
          Nothing  -> throwError (errorMessage (UnknownClass className'))
          Just tcd -> pure tcd

        -- Get current instance context from the StateT InstanceContext layer
        currentInstanceCtx <- lift get

        -- Build combined instance context
        let envInstanceCtx = arrayCtxToInstanceCtx envR.typeClassDictionaries
            passedCtx      = arrayCtxToInstanceCtx context
            combinedCtx    = combineContexts passedCtx (combineContexts envInstanceCtx currentInstanceCtx)

        dicts <- lift $ lift $ forClassNameM env combinedCtx className' kinds'' tys''

        -- Group chains and find matches
        let sortedDicts = Array.sortBy
              (\(TypeClassDictionaryInScope d1) (TypeClassDictionaryInScope d2) ->
                compare (Tuple d1.tcdChain d1.tcdIndex) (Tuple d2.tcdChain d2.tcdIndex))
              dicts
            chainGrouped = groupByChain sortedDicts
            candidates = Array.concatMap processChain chainGrouped
            (ambiguous :: Array (Qualified (Either SourceType Ident)))
              = Array.catMaybes (map fst candidates)
            instances = Array.catMaybes (map snd candidates)

        solution <- lift $ lift $
          uniqueSolution kinds'' tys'' ambiguous instances
            (unknownsInAllCoveringSets
              (\i -> fst (fromMaybe (Tuple "" Nothing) (Array.index tcData.typeClassArguments i)))
              tcData.typeClassMembers
              tys''
              tcData.typeClassCoveringSets)

        case solution of
          Solved substs tcd -> do
            let TypeClassDictionaryInScope tcdR = tcd
            result <- lift $ lift do
              -- Make sure the substitution is valid: unify pairwise
              for_ substs pairwiseUnify
              -- Apply functional dependencies via fresh types
              let subst = map (\arr -> fromMaybe (internalCompilerErrorPure "entails: empty substitution") (Array.head arr)) substs
              currentSubst <- gets \(CheckState s) -> s.checkSubstitution
              subst' <- withFreshTypes tcd (map (substituteType currentSubst) subst)
              -- Unify instance types with constraint types
              traverse_ (\(Tuple t1 t2) -> do
                let inferredType = replaceAllTypeVars (Map.toUnfoldable subst') t1
                unifyTypes inferredType t2)
                (Array.zip tcdR.tcdInstanceTypes tys'')
              currentSubst' <- gets \(CheckState s) -> s.checkSubstitution
              let subst'' = map (substituteType currentSubst') subst'
              -- Solve subgoals
              args <- solveSubgoals subst'' (ErrorSolvingConstraint con) tcdR.tcdDependencies
              initDict <- mkDictionary tcdR.tcdValue args
              let match = foldr (\(Tuple cn idx) dict -> subclassDictionaryValue dict cn idx) initDict tcdR.tcdPath
              pure (if tcData.typeClassIsEmpty then Unused match else match)
            tell (Tuple (Disj true) [])
            pure result

          Unsolved unsolved -> do
            let Constraint unsolvedR = unsolved
            ident <- lift $ lift $ freshIdent ("dict" <> runProperName (disqualify unsolvedR.constraintClass))
            let qident = Qualified byNullSourcePos ident
            newDicts <- lift $ lift $ newDictionaries [] qident unsolved
            let newContext = mkContext newDicts
            lift $ modify_ (combineContexts newContext)
            tell (Tuple (Disj false) [Tuple ident (Tuple (arrayCtxToInstanceCtx context) unsolved)])
            pure (Var nullSourceSpan qident)

          Deferred ->
            pure (TypeClassDictionary (srcConstraint className' kinds'' tys'' conInfo) context hints')

      pairwiseUnify :: Array SourceType -> m Unit
      pairwiseUnify types = case Array.uncons types of
        Nothing -> pure unit
        Just { head: x, tail: xs } -> do
          traverse_ (unifyTypes x) xs
          pairwiseUnify xs

      withFreshTypes
        :: TypeClassDict
        -> Matching SourceType
        -> m (Matching SourceType)
      withFreshTypes (TypeClassDictionaryInScope tcdR) initSubst = do
        subst <- foldM addFresh initSubst
                  (Array.filter (\(Tuple v _) -> not (Map.member v initSubst)) tcdR.tcdForAll)
        for_ (Map.toUnfoldable initSubst :: Array (Tuple String SourceType)) (unifySubstKind subst)
        pure subst
        where
        addFresh subst (Tuple var kind) = do
          ty <- freshTypeWithKind (replaceAllTypeVars (Map.toUnfoldable subst) kind)
          pure (Map.insert var ty subst)

        unifySubstKind subst (Tuple var ty) =
          case Array.find (\(Tuple v _) -> v == var) tcdR.tcdForAll of
            Nothing -> pure unit
            Just (Tuple _ instKind) -> do
              tyKind <- elaborateKind ty
              currentSubst <- gets \(CheckState s) -> s.checkSubstitution
              unifyKinds'
                (substituteType currentSubst (replaceAllTypeVars (Map.toUnfoldable subst) instKind))
                (substituteType currentSubst tyKind)

      uniqueSolution
        :: Array SourceType
        -> Array SourceType
        -> Array (Qualified (Either SourceType Ident))
        -> Array (Tuple (Matching (Array SourceType)) TypeClassDict)
        -> UnknownsHint
        -> m (EntailsResult (Matching (Array SourceType)))
      uniqueSolution kindArgs tyArgs ambiguous [] unks
        | opts.solverDeferErrors = pure Deferred
        | opts.solverShouldGeneralize &&
            ((Array.null kindArgs && Array.null tyArgs)
              || Array.any canBeGeneralized kindArgs
              || Array.any canBeGeneralized tyArgs) =
              pure (Unsolved (srcConstraint c.constraintClass kindArgs tyArgs c.constraintData))
        | otherwise = throwError (errorMessage (NoInstanceFound (srcConstraint c.constraintClass kindArgs tyArgs c.constraintData) ambiguous unks))
      uniqueSolution _ _ _ [Tuple substs dict] _ = pure (Solved substs dict)
      uniqueSolution _ tyArgs _ tcds _
        | pairwiseAny overlapping (map snd tcds) =
            throwError (errorMessage (OverlappingInstances c.constraintClass tyArgs
              (Array.concatMap (\(Tuple _ d) -> Array.catMaybes [tcdToInstanceDescription d]) tcds)))
        | otherwise =
            let Tuple minSubsts minDict = minimumByPathLength tcds
            in pure (Solved minSubsts minDict)

      tcdToInstanceDescription :: TypeClassDict -> Maybe (Qualified (Either SourceType Ident))
      tcdToInstanceDescription (TypeClassDictionaryInScope tcdR) =
        let nii = namedInstanceIdentifier tcdR.tcdValue
        in case tcdR.tcdDescription of
          Just ty -> flip Qualified (Left ty) <$> (map (byMaybeModuleName <<< getQual) nii)
          Nothing -> map Right <$> nii

      canBeGeneralized :: forall a. Type a -> Boolean
      canBeGeneralized (TUnknown _ _)    = true
      canBeGeneralized (KindedType _ t _) = canBeGeneralized t
      canBeGeneralized _                 = false

      overlapping :: TypeClassDict -> TypeClassDict -> Boolean
      overlapping (TypeClassDictionaryInScope d1) _ | not (Array.null d1.tcdPath) = false
      overlapping _ (TypeClassDictionaryInScope d2) | not (Array.null d2.tcdPath) = false
      overlapping (TypeClassDictionaryInScope d1) _ | isNothing d1.tcdDependencies = false
      overlapping _ (TypeClassDictionaryInScope d2) | isNothing d2.tcdDependencies = false
      overlapping (TypeClassDictionaryInScope d1) (TypeClassDictionaryInScope d2) =
        d1.tcdValue /= d2.tcdValue

      minimumByPathLength
        :: Array (Tuple (Matching (Array SourceType)) TypeClassDict)
        -> Tuple (Matching (Array SourceType)) TypeClassDict
      minimumByPathLength arr =
        fromMaybe (internalCompilerErrorPure "minimumByPathLength: empty") $
          Array.foldl
            (\acc x ->
              case acc of
                Nothing -> Just x
                Just prev ->
                  let TypeClassDictionaryInScope d1 = snd prev
                      TypeClassDictionaryInScope d2 = snd x
                  in if Array.length d1.tcdPath <= Array.length d2.tcdPath then Just prev else Just x)
            Nothing
            arr

      solveSubgoals
        :: Matching SourceType
        -> ErrorMessageHint
        -> Maybe (Array SourceConstraint)
        -> m (Maybe (Array Expr))
      solveSubgoals _ _ Nothing = pure Nothing
      solveSubgoals subst hint (Just subgoals) = do
        args <- for subgoals \sg -> do
          let sg' = mapConstraintArgsAll (map (replaceAllTypeVars (Map.toUnfoldable subst))) sg
          -- We need to recursively solve; do it as a sub-entailment
          rethrow (addHint hint) (solveSubgoalConstraint (work + 1) (hints' <> [hint]) sg')
        pure (Just args)

      solveSubgoalConstraint
        :: Int
        -> Array ErrorMessageHint
        -> SourceConstraint
        -> m Expr
      solveSubgoalConstraint w hs sg = do
        -- Run the inner go through StateT/WriterT
        Tuple (Tuple result _) _ <- runStateT (runWriterT (go w hs sg)) Map.empty
        pure result

      useEmptyDict :: Maybe (Array Expr) -> Expr
      useEmptyDict args =
        Unused (foldl (\acc argExpr -> App (Abs (VarBinder nullSourceSpan UnusedIdent) acc) argExpr)
                  valUndefined
                  (fromMaybe [] args))

      unknownsInAllCoveringSets
        :: (Int -> String)
        -> Array (Tuple (Tuple Ident SourceType) (Maybe (Set (Array Int))))
        -> Array SourceType
        -> Set (Set Int)
        -> UnknownsHint
      unknownsInAllCoveringSets indexToArgText _tyClassMembers tyArgs coveringSets =
        let unkIndices = map snd
              $ Array.filter (containsUnknowns_ <<< fst)
              $ Array.zip tyArgs (Array.range 0 (Array.length tyArgs - 1))
        in if Array.all (\s -> Array.any (\i -> Set.member i s) unkIndices) (Array.fromFoldable coveringSets)
           then Unknowns
           else NoUnknowns

      containsUnknowns_ :: SourceType -> Boolean
      containsUnknowns_ = everythingOnTypes (||) (\ty -> case ty of
        TUnknown _ _ -> true
        _            -> false)

  -- | Turn a DictionaryValue into a Expr
  subclassDictionaryValue :: Expr -> Qualified (ProperName ClassName) -> Int -> Expr
  subclassDictionaryValue dict className idx =
    App (Accessor (mkString (superclassName className idx)) dict) valUndefined

  valUndefined :: Expr
  valUndefined = Var nullSourceSpan (Qualified (ByModuleName (ModuleName "Prim")) (Ident "undefined"))

  mkDictionary :: Evidence -> Maybe (Array Expr) -> m Expr
  mkDictionary (NamedInstance n) args =
    pure (foldl App (Var nullSourceSpan n) (fromMaybe [] args))
  mkDictionary EmptyClassInstance args =
    pure (useEmptyDict args)
    where
    useEmptyDict :: Maybe (Array Expr) -> Expr
    useEmptyDict a =
      Unused (foldl (\acc argExpr -> App (Abs (VarBinder nullSourceSpan UnusedIdent) acc) argExpr)
                valUndefined
                (fromMaybe [] a))
  mkDictionary (WarnInstance msg) args = do
    tell (errorMessage (UserDefinedWarning msg))
    pure (useEmptyDictSimple args)
    where
    useEmptyDictSimple :: Maybe (Array Expr) -> Expr
    useEmptyDictSimple a =
      Unused (foldl (\acc argExpr -> App (Abs (VarBinder nullSourceSpan UnusedIdent) acc) argExpr)
                valUndefined
                (fromMaybe [] a))
  mkDictionary (IsSymbolInstance sym) _ =
    let fields = [Tuple (mkString "reflectSymbol") (Abs (VarBinder nullSourceSpan UnusedIdent) (Literal nullSourceSpan (StringLiteral sym)))]
    in pure (App (Constructor nullSourceSpan (coerceProperName <$> dictTypeName <$> isSymbolClass))
                 (Literal nullSourceSpan (ObjectLiteral fields)))
    where
    isSymbolClass :: Qualified (ProperName ClassName)
    isSymbolClass = Qualified (ByModuleName (ModuleName "Data.Symbol")) (ProperName "IsSymbol")
  mkDictionary (ReflectableInstance ref) _ =
    let fields = [Tuple (mkString "reflectType") (Abs (VarBinder nullSourceSpan UnusedIdent) (asExpression ref))]
        reflectableClass = Qualified (ByModuleName (ModuleName "Data.Reflectable")) (ProperName "Reflectable")
    in pure (App (Constructor nullSourceSpan (coerceProperName <$> dictTypeName <$> reflectableClass))
                 (Literal nullSourceSpan (ObjectLiteral fields)))

  forClassNameM
    :: Environment
    -> InstanceContext
    -> Qualified (ProperName ClassName)
    -> Array SourceType
    -> Array SourceType
    -> m (Array TypeClassDict)
  forClassNameM env ctx cn kinds args
    | cn == C.tyCoercible = do
        mbDicts <- solveCoercible env ctx kinds args
        pure (fromMaybe (forClassName env ctx cn kinds args) mbDicts)
    | otherwise = pure (forClassName env ctx cn kinds args)

  forClassName
    :: Environment
    -> InstanceContext
    -> Qualified (ProperName ClassName)
    -> Array SourceType
    -> Array SourceType
    -> Array TypeClassDict
  forClassName env ctx cn kinds args =
    let isSymbolClass    = Qualified (ByModuleName (ModuleName "Data.Symbol")) (ProperName "IsSymbol")
        reflectableClass = Qualified (ByModuleName (ModuleName "Data.Reflectable")) (ProperName "Reflectable")
    in if cn == C.clsWarn then
         case args of
           [msg] -> findDicts ctx cn byNullSourcePos
             <> [TypeClassDictionaryInScope
                   { tcdChain: Nothing, tcdIndex: 0, tcdValue: WarnInstance msg
                   , tcdPath: [], tcdClassName: cn, tcdForAll: []
                   , tcdInstanceKinds: [], tcdInstanceTypes: [msg]
                   , tcdDependencies: Nothing, tcdDescription: Nothing }]
           _ -> []
       else if cn == isSymbolClass then
         fromMaybe [] (solveIsSymbol args)
       else if cn == C.clsSymbolCompare then
         fromMaybe [] (solveSymbolCompare args)
       else if cn == C.clsSymbolAppend then
         fromMaybe [] (solveSymbolAppend args)
       else if cn == C.clsSymbolCons then
         fromMaybe [] (solveSymbolCons args)
       else if cn == C.clsIntAdd then
         fromMaybe [] (solveIntAdd args)
       else if cn == C.clsIntCompare then
         fromMaybe [] (solveIntCompare ctx args)
       else if cn == C.clsIntMul then
         fromMaybe [] (solveIntMul args)
       else if cn == C.clsIntToString then
         fromMaybe [] (solveIntToString args)
       else if cn == reflectableClass then
         fromMaybe [] (solveReflectable args)
       else if cn == C.clsRowUnion then
         fromMaybe [] (solveUnion kinds args)
       else if cn == C.clsRowNub then
         fromMaybe [] (solveNub kinds args)
       else if cn == C.clsRowLacks then
         fromMaybe [] (solveLacks kinds args)
       else if cn == C.clsRowCons then
         fromMaybe [] (solveRowCons kinds args)
       else if cn == C.clsRowToList then
         fromMaybe [] (solveRowToList kinds args)
       else
         case cn of
           Qualified (ByModuleName mn) _ ->
             let moduleQBs = Array.nub (Array.cons byNullSourcePos (Array.cons (ByModuleName mn) (Array.mapMaybe ctorModules args)))
             in Array.concatMap (findDicts ctx cn) moduleQBs
           _ -> []

  ctorModules :: SourceType -> Maybe QualifiedBy
  ctorModules (TypeConstructor _ (Qualified (ByModuleName mn) _)) = Just (ByModuleName mn)
  ctorModules (TypeConstructor _ (Qualified (BySourcePos _) _))   = Nothing
  ctorModules (TypeApp _ ty _)   = ctorModules ty
  ctorModules (KindApp _ ty _)   = ctorModules ty
  ctorModules (KindedType _ ty _) = ctorModules ty
  ctorModules _ = Nothing

  solveCoercible
    :: Environment
    -> InstanceContext
    -> Array SourceType
    -> Array SourceType
    -> m (Maybe (Array TypeClassDict))
  solveCoercible env ctx kinds [a, b] = do
    let coercibleDictsInScope = findDicts ctx C.tyCoercible byNullSourcePos
        givens = Array.catMaybes $ map (\(TypeClassDictionaryInScope d) ->
          case d.tcdInstanceTypes of
            [a', b'] -> Just (Tuple a' b')
            _        -> Nothing) coercibleDictsInScope
    GivenSolverState gsState <- execStateT (solveGivens env) (initialGivenSolverState givens)
    Tuple (WantedSolverState wsState) hints' <- runWriterT $
      execStateT (solveWanteds env :: StateT WantedSolverState (WriterT MultipleErrors m) Unit) (initialWantedSolverState gsState.inertGivens a b)
    -- TODO: full implementation; for now optimistically succeed
    case wsState.inertWanteds of
      [] -> pure (Just [TypeClassDictionaryInScope
              { tcdChain: Nothing, tcdIndex: 0, tcdValue: EmptyClassInstance
              , tcdPath: [], tcdClassName: C.tyCoercible, tcdForAll: []
              , tcdInstanceKinds: kinds, tcdInstanceTypes: [a, b]
              , tcdDependencies: Nothing, tcdDescription: Nothing }])
      _ -> case Array.head wsState.inertWanteds of
        Just (Tuple _ (Tuple a' b')) ->
          throwError (insoluble (TypeConstructor nullSourceAnn (map coerceProperName C.tyCoercible)) a' b')
        Nothing -> pure Nothing
  solveCoercible _ _ _ _ = pure Nothing

  solveIsSymbol :: Array SourceType -> Maybe (Array TypeClassDict)
  solveIsSymbol [TypeLevelString ann sym] =
    let cn = Qualified (ByModuleName (ModuleName "Data.Symbol")) (ProperName "IsSymbol")
    in Just [TypeClassDictionaryInScope
              { tcdChain: Nothing, tcdIndex: 0, tcdValue: IsSymbolInstance sym
              , tcdPath: [], tcdClassName: cn, tcdForAll: []
              , tcdInstanceKinds: [], tcdInstanceTypes: [TypeLevelString ann sym]
              , tcdDependencies: Nothing, tcdDescription: Nothing }]
  solveIsSymbol _ = Nothing

  solveSymbolCompare :: Array SourceType -> Maybe (Array TypeClassDict)
  solveSymbolCompare [arg0@(TypeLevelString _ lhs), arg1@(TypeLevelString _ rhs), _] =
    let ordering = case compare lhs rhs of
                     LT -> C.tyLT
                     EQ -> C.tyEQ
                     GT -> C.tyGT
        args' = [arg0, arg1, srcTypeConstructor ordering]
    in Just [TypeClassDictionaryInScope
              { tcdChain: Nothing, tcdIndex: 0, tcdValue: EmptyClassInstance
              , tcdPath: [], tcdClassName: C.clsSymbolCompare, tcdForAll: []
              , tcdInstanceKinds: [], tcdInstanceTypes: args'
              , tcdDependencies: Nothing, tcdDescription: Nothing }]
  solveSymbolCompare _ = Nothing

  solveSymbolAppend :: Array SourceType -> Maybe (Array TypeClassDict)
  solveSymbolAppend [arg0, arg1, arg2] = do
    Tuple (Tuple arg0' arg1') arg2' <- appendSymbols arg0 arg1 arg2
    let args' = [arg0', arg1', arg2']
    pure [TypeClassDictionaryInScope
           { tcdChain: Nothing, tcdIndex: 0, tcdValue: EmptyClassInstance
           , tcdPath: [], tcdClassName: C.clsSymbolAppend, tcdForAll: []
           , tcdInstanceKinds: [], tcdInstanceTypes: args'
           , tcdDependencies: Nothing, tcdDescription: Nothing }]
  solveSymbolAppend _ = Nothing

  appendSymbols
    :: SourceType -> SourceType -> SourceType
    -> Maybe (Tuple (Tuple SourceType SourceType) SourceType)
  appendSymbols arg0@(TypeLevelString _ lhs) arg1@(TypeLevelString _ rhs) _ =
    Just (Tuple (Tuple arg0 arg1) (srcTypeLevelString (lhs <> rhs)))
  appendSymbols arg0@(TypeLevelString _ lhs) _ arg2@(TypeLevelString _ out) = do
    lhs' <- decodeString lhs
    out' <- decodeString out
    rhs  <- stripPrefix (Pattern lhs') out'
    pure (Tuple (Tuple arg0 (srcTypeLevelString (mkString rhs))) arg2)
  appendSymbols _ arg1@(TypeLevelString _ rhs) arg2@(TypeLevelString _ out) = do
    rhs' <- decodeString rhs
    out' <- decodeString out
    lhs  <- stripSuffix (Pattern rhs') out'
    pure (Tuple (Tuple (srcTypeLevelString (mkString lhs)) arg1) arg2)
  appendSymbols _ _ _ = Nothing

  solveSymbolCons :: Array SourceType -> Maybe (Array TypeClassDict)
  solveSymbolCons [arg0, arg1, arg2] = do
    Tuple (Tuple arg0' arg1') arg2' <- consSymbol arg0 arg1 arg2
    let args' = [arg0', arg1', arg2']
    pure [TypeClassDictionaryInScope
           { tcdChain: Nothing, tcdIndex: 0, tcdValue: EmptyClassInstance
           , tcdPath: [], tcdClassName: C.clsSymbolCons, tcdForAll: []
           , tcdInstanceKinds: [], tcdInstanceTypes: args'
           , tcdDependencies: Nothing, tcdDescription: Nothing }]
  solveSymbolCons _ = Nothing

  consSymbol
    :: SourceType -> SourceType -> SourceType
    -> Maybe (Tuple (Tuple SourceType SourceType) SourceType)
  consSymbol _ _ arg@(TypeLevelString _ s) = do
    s' <- decodeString s
    case String.uncons s' of
      Nothing -> Nothing
      Just { head: h, tail: t } ->
        let mkTLS str = srcTypeLevelString (mkString str)
        in Just (Tuple (Tuple (mkTLS (String.singleton h)) (mkTLS t)) arg)
  consSymbol arg1@(TypeLevelString _ h) arg2@(TypeLevelString _ t) _ = do
    h' <- decodeString h
    t' <- decodeString t
    if String.length h' == 1
      then Just (Tuple (Tuple arg1 arg2) (srcTypeLevelString (mkString (h' <> t'))))
      else Nothing
  consSymbol _ _ _ = Nothing

  solveIntToString :: Array SourceType -> Maybe (Array TypeClassDict)
  solveIntToString [arg0, _] = do
    Tuple arg0' arg1' <- printIntToString arg0
    let args' = [arg0', arg1']
    pure [TypeClassDictionaryInScope
           { tcdChain: Nothing, tcdIndex: 0, tcdValue: EmptyClassInstance
           , tcdPath: [], tcdClassName: C.clsIntToString, tcdForAll: []
           , tcdInstanceKinds: [], tcdInstanceTypes: args'
           , tcdDependencies: Nothing, tcdDescription: Nothing }]
  solveIntToString _ = Nothing

  printIntToString :: SourceType -> Maybe (Tuple SourceType SourceType)
  printIntToString arg0@(TypeLevelInt _ i) =
    Just (Tuple arg0 (srcTypeLevelString (mkString (show i))))
  printIntToString _ = Nothing

  solveReflectable :: Array SourceType -> Maybe (Array TypeClassDict)
  solveReflectable [typeLevel, _] = do
    Tuple ref typ <- case typeLevel of
      TypeLevelInt _ i    -> Just (Tuple (ReflectableInt i)    (srcTypeConstructor C.tyInt))
      TypeLevelString _ s -> Just (Tuple (ReflectableString s) (srcTypeConstructor C.tyString))
      TypeConstructor _ n
        | n == C.tyTrue    -> Just (Tuple (ReflectableBoolean true)  (srcTypeConstructor C.tyBoolean))
        | n == C.tyFalse   -> Just (Tuple (ReflectableBoolean false) (srcTypeConstructor C.tyBoolean))
        | n == C.tyLT      -> Just (Tuple (ReflectableOrdering LT)   (srcTypeConstructor C.tyTypeOrdering))
        | n == C.tyEQ      -> Just (Tuple (ReflectableOrdering EQ)   (srcTypeConstructor C.tyTypeOrdering))
        | n == C.tyGT      -> Just (Tuple (ReflectableOrdering GT)   (srcTypeConstructor C.tyTypeOrdering))
        | otherwise        -> Nothing
      _ -> Nothing
    let reflectableClass = Qualified (ByModuleName (ModuleName "Data.Reflectable")) (ProperName "Reflectable")
    pure [TypeClassDictionaryInScope
           { tcdChain: Nothing, tcdIndex: 0, tcdValue: ReflectableInstance ref
           , tcdPath: [], tcdClassName: reflectableClass, tcdForAll: []
           , tcdInstanceKinds: [], tcdInstanceTypes: [typeLevel, typ]
           , tcdDependencies: Nothing, tcdDescription: Nothing }]
  solveReflectable _ = Nothing

  solveIntAdd :: Array SourceType -> Maybe (Array TypeClassDict)
  solveIntAdd [arg0, arg1, arg2] = do
    Tuple (Tuple arg0' arg1') arg2' <- addInts arg0 arg1 arg2
    let args' = [arg0', arg1', arg2']
    pure [TypeClassDictionaryInScope
           { tcdChain: Nothing, tcdIndex: 0, tcdValue: EmptyClassInstance
           , tcdPath: [], tcdClassName: C.clsIntAdd, tcdForAll: []
           , tcdInstanceKinds: [], tcdInstanceTypes: args'
           , tcdDependencies: Nothing, tcdDescription: Nothing }]
  solveIntAdd _ = Nothing

  addInts
    :: SourceType -> SourceType -> SourceType
    -> Maybe (Tuple (Tuple SourceType SourceType) SourceType)
  -- l r -> o
  addInts arg0@(TypeLevelInt _ l) arg1@(TypeLevelInt _ r) _ =
    Just (Tuple (Tuple arg0 arg1) (srcTypeLevelInt (l + r)))
  -- l o -> r
  addInts arg0@(TypeLevelInt _ l) _ arg2@(TypeLevelInt _ o) =
    Just (Tuple (Tuple arg0 (srcTypeLevelInt (o - l))) arg2)
  -- r o -> l
  addInts _ arg1@(TypeLevelInt _ r) arg2@(TypeLevelInt _ o) =
    Just (Tuple (Tuple (srcTypeLevelInt (o - r)) arg1) arg2)
  addInts _ _ _ = Nothing

  solveIntCompare :: InstanceContext -> Array SourceType -> Maybe (Array TypeClassDict)
  solveIntCompare _ [arg0@(TypeLevelInt _ a), arg1@(TypeLevelInt _ b), _] =
    let ordering = case compare a b of
                     EQ -> C.tyEQ
                     LT -> C.tyLT
                     GT -> C.tyGT
        args' = [arg0, arg1, srcTypeConstructor ordering]
    in pure [TypeClassDictionaryInScope
               { tcdChain: Nothing, tcdIndex: 0, tcdValue: EmptyClassInstance
               , tcdPath: [], tcdClassName: C.clsIntCompare, tcdForAll: []
               , tcdInstanceKinds: [], tcdInstanceTypes: args'
               , tcdDependencies: Nothing, tcdDescription: Nothing }]
  solveIntCompare ctx args@[a, b, _] = do
    let compareDictsInScope = findDicts ctx C.clsIntCompare byNullSourcePos
        givens = Array.catMaybes $ map (\(TypeClassDictionaryInScope d) ->
          case d.tcdInstanceTypes of
            [a', b', c'] -> mkRelation a' b' c'
            _            -> Nothing) compareDictsInScope
        facts  = mkFacts (Array.cons args (map (\(TypeClassDictionaryInScope d) -> d.tcdInstanceTypes) compareDictsInScope))
    c' <- solveRelation (givens <> facts) a b
    pure [TypeClassDictionaryInScope
            { tcdChain: Nothing, tcdIndex: 0, tcdValue: EmptyClassInstance
            , tcdPath: [], tcdClassName: C.clsIntCompare, tcdForAll: []
            , tcdInstanceKinds: [], tcdInstanceTypes: [a, b, srcTypeConstructor c']
            , tcdDependencies: Nothing, tcdDescription: Nothing }]
  solveIntCompare _ _ = Nothing

  solveIntMul :: Array SourceType -> Maybe (Array TypeClassDict)
  solveIntMul [arg0@(TypeLevelInt _ l), arg1@(TypeLevelInt _ r), _] =
    let args' = [arg0, arg1, srcTypeLevelInt (l * r)]
    in pure [TypeClassDictionaryInScope
               { tcdChain: Nothing, tcdIndex: 0, tcdValue: EmptyClassInstance
               , tcdPath: [], tcdClassName: C.clsIntMul, tcdForAll: []
               , tcdInstanceKinds: [], tcdInstanceTypes: args'
               , tcdDependencies: Nothing, tcdDescription: Nothing }]
  solveIntMul _ = Nothing

  solveUnion :: Array SourceType -> Array SourceType -> Maybe (Array TypeClassDict)
  solveUnion kinds [l, r, u] = do
    Tuple (Tuple (Tuple lOut rOut) uOut) mbCst <- unionRows kinds l r u
    let vars = case mbCst of
                 Just _ ->
                   let rowKind = fromMaybe (internalCompilerErrorPure "solveUnion: empty kinds") (Array.head kinds)
                   in [Tuple "r" (srcKindApp (srcTypeConstructor C.tyRow) rowKind)]
                 Nothing -> []
    pure [TypeClassDictionaryInScope
            { tcdChain: Nothing, tcdIndex: 0, tcdValue: EmptyClassInstance
            , tcdPath: [], tcdClassName: C.clsRowUnion, tcdForAll: vars
            , tcdInstanceKinds: kinds, tcdInstanceTypes: [lOut, rOut, uOut]
            , tcdDependencies: mbCst, tcdDescription: Nothing }]
  solveUnion _ _ = Nothing

  unionRows
    :: Array SourceType
    -> SourceType
    -> SourceType
    -> SourceType
    -> Maybe (Tuple (Tuple (Tuple SourceType SourceType) SourceType) (Maybe (Array SourceConstraint)))
  unionRows kinds l r u =
    let Tuple fixed rest = rowToList l
        rowVar = srcTypeVar "r"
    in case rest of
      REmpty _ ->
        -- Left is closed: merge labels into right
        Just (Tuple (Tuple (Tuple l r) (rowFromList (Tuple fixed r))) Nothing)
      KindApp _ (REmpty _) _ ->
        Just (Tuple (Tuple (Tuple l r) (rowFromList (Tuple fixed r))) Nothing)
      _ ->
        let Tuple right rightu = rowToList r
            Tuple output restu = rowToList u
        in if isREmpty rightu && isREmpty restu then
          -- Compute left by subtracting right from output
          let rightLabels = map (\(RowListItem item) -> item.rowListLabel) right
              grabLabel e (Tuple (Tuple left' right') remaining) =
                let RowListItem eR = e
                in if Array.elem eR.rowListLabel remaining
                   then Tuple (Tuple left' (Array.cons e right')) (Array.delete eR.rowListLabel remaining)
                   else Tuple (Tuple (Array.cons e left') right') remaining
              Tuple (Tuple outL outR) leftover =
                foldr grabLabel (Tuple (Tuple [] []) rightLabels) output
          in if Array.null leftover
             then Just (Tuple (Tuple (Tuple (rowFromList (Tuple outL restu)) (rowFromList (Tuple outR rightu))) u) Nothing)
             else Nothing
        else
          -- Move known labels from left into output, constraint for rest
          if Array.null fixed
          then Nothing
          else Just (Tuple (Tuple (Tuple l r) (rowFromList (Tuple fixed rowVar)))
                           (Just [srcConstraint C.clsRowUnion kinds [rest, r, rowVar] Nothing]))

  solveRowCons :: Array SourceType -> Array SourceType -> Maybe (Array TypeClassDict)
  solveRowCons kinds [TypeLevelString ann sym, ty, r, _] =
    Just [TypeClassDictionaryInScope
            { tcdChain: Nothing, tcdIndex: 0, tcdValue: EmptyClassInstance
            , tcdPath: [], tcdClassName: C.clsRowCons, tcdForAll: []
            , tcdInstanceKinds: kinds, tcdInstanceTypes: [TypeLevelString ann sym, ty, r, srcRCons (Label sym) ty r]
            , tcdDependencies: Nothing, tcdDescription: Nothing }]
  solveRowCons _ _ = Nothing

  solveRowToList :: Array SourceType -> Array SourceType -> Maybe (Array TypeClassDict)
  solveRowToList [kind] [r, _] = do
    entries <- rowToRowList kind r
    pure [TypeClassDictionaryInScope
            { tcdChain: Nothing, tcdIndex: 0, tcdValue: EmptyClassInstance
            , tcdPath: [], tcdClassName: C.clsRowToList, tcdForAll: []
            , tcdInstanceKinds: [kind], tcdInstanceTypes: [r, entries]
            , tcdDependencies: Nothing, tcdDescription: Nothing }]
  solveRowToList _ _ = Nothing

  rowToRowList :: SourceType -> SourceType -> Maybe SourceType
  rowToRowList kind r =
    let Tuple fixed rest = rowToSortedList r
    in if isREmpty rest
       then Just (foldr rowListCons (srcKindApp (srcTypeConstructor C.tyRowListNil) kind) fixed)
       else Nothing
    where
    rowListCons (RowListItem item) tl =
      foldl srcTypeApp (srcKindApp (srcTypeConstructor C.tyRowListCons) kind)
        [ srcTypeLevelString (runLabel item.rowListLabel)
        , item.rowListType
        , tl ]

  solveNub :: Array SourceType -> Array SourceType -> Maybe (Array TypeClassDict)
  solveNub kinds [r, _] = do
    r' <- nubRows r
    pure [TypeClassDictionaryInScope
            { tcdChain: Nothing, tcdIndex: 0, tcdValue: EmptyClassInstance
            , tcdPath: [], tcdClassName: C.clsRowNub, tcdForAll: []
            , tcdInstanceKinds: kinds, tcdInstanceTypes: [r, r']
            , tcdDependencies: Nothing, tcdDescription: Nothing }]
  solveNub _ _ = Nothing

  nubRows :: SourceType -> Maybe SourceType
  nubRows r =
    let Tuple fixed rest = rowToSortedList r
    in if isREmpty rest
       then Just (rowFromList (Tuple (nubByLabel fixed) rest))
       else Nothing

  nubByLabel :: Array (RowListItem SourceAnn) -> Array (RowListItem SourceAnn)
  nubByLabel = Array.nubByEq \(RowListItem a) (RowListItem b) -> a.rowListLabel == b.rowListLabel

  solveLacks :: Array SourceType -> Array SourceType -> Maybe (Array TypeClassDict)
  solveLacks kinds tys@[_, r] | isREmpty r =
    pure [TypeClassDictionaryInScope
            { tcdChain: Nothing, tcdIndex: 0, tcdValue: EmptyClassInstance
            , tcdPath: [], tcdClassName: C.clsRowLacks, tcdForAll: []
            , tcdInstanceKinds: kinds, tcdInstanceTypes: tys
            , tcdDependencies: Nothing, tcdDescription: Nothing }]
  solveLacks kinds [TypeLevelString ann sym, r] = do
    Tuple r' mbCst <- rowLacks kinds sym r
    pure [TypeClassDictionaryInScope
            { tcdChain: Nothing, tcdIndex: 0, tcdValue: EmptyClassInstance
            , tcdPath: [], tcdClassName: C.clsRowLacks, tcdForAll: []
            , tcdInstanceKinds: kinds, tcdInstanceTypes: [TypeLevelString ann sym, r']
            , tcdDependencies: mbCst, tcdDescription: Nothing }]
  solveLacks _ _ = Nothing

  rowLacks
    :: Array SourceType
    -> PSString
    -> SourceType
    -> Maybe (Tuple SourceType (Maybe (Array SourceConstraint)))
  rowLacks kinds sym r =
    let Tuple fixed rest = rowToList r
        lacksSym = sym `Array.notElem` map (\(RowListItem item) -> runLabel item.rowListLabel) fixed
    in if not lacksSym
       then Nothing
       else case rest of
         REmpty _ -> Just (Tuple r Nothing)
         KindApp _ (REmpty _) _ -> Just (Tuple r Nothing)
         _ ->
           if Array.null fixed
           then Nothing
           else Just (Tuple r (Just [srcConstraint C.clsRowLacks kinds [srcTypeLevelString sym, rest] Nothing]))

-- | Check if an instance matches our list of types
matches
  :: Array FunctionalDependency
  -> TypeClassDict
  -> Array SourceType
  -> Matched (Matching (Array SourceType))
matches deps (TypeClassDictionaryInScope tcdR) tys =
  let matched = Array.zipWith typeHeadsAreEqual tys tcdR.tcdInstanceTypes
  in if not (covers matched)
     then if Array.any (\(Tuple m _) -> m == Apart) matched then Apart else Unknown
     else
       let determinedSet = foldl (\s (FunctionalDependency d) -> s <> Set.fromFoldable d.fdDetermined) Set.empty deps
           indexedSubsts = Array.zipWith
               (\i mPair -> Tuple i (snd mPair))
               (Array.range 0 (Array.length matched - 1))
               matched
           solved = map (\(Tuple _ subst) -> subst)
                  $ Array.filter (\(Tuple i _) -> not (Set.member i determinedSet))
                  $ indexedSubsts
       in verifySubstitution (foldl (Map.unionWith (<>)) Map.empty solved)
  where
  covers :: Array (Tuple (Matched Unit) (Matching (Array SourceType))) -> Boolean
  covers ms =
    let initialSet = Set.fromFoldable $ map snd $ Array.filter (\(Tuple (Tuple m _) _) -> m == Match unit) $ Array.zip ms (Array.range 0 (Array.length ms - 1))
        finalSet   = untilFixedPoint (applyAll deps) initialSet
    in finalSet == Set.fromFoldable (Array.range 0 (Array.length ms - 1))

  untilFixedPoint :: forall a. Eq a => (a -> a) -> a -> a
  untilFixedPoint f x =
    let x' = f x
    in if x' == x then x' else untilFixedPoint f x'

  applyAll :: Array FunctionalDependency -> Set Int -> Set Int
  applyAll fds s = foldl applyDep s fds

  applyDep :: Set Int -> FunctionalDependency -> Set Int
  applyDep xs (FunctionalDependency d) =
    if Set.fromFoldable d.fdDeterminers `Set.subset` xs
    then xs <> Set.fromFoldable d.fdDetermined
    else xs

  typeHeadsAreEqual
    :: forall a. Eq a => Type a -> Type a
    -> Tuple (Matched Unit) (Matching (Array (Type a)))
  typeHeadsAreEqual (KindedType _ t1 _) t2 = typeHeadsAreEqual t1 t2
  typeHeadsAreEqual t1 (KindedType _ t2 _) = typeHeadsAreEqual t1 t2
  typeHeadsAreEqual (TUnknown _ u1) (TUnknown _ u2) | u1 == u2 = Tuple (Match unit) Map.empty
  typeHeadsAreEqual (Skolem _ _ _ s1 _) (Skolem _ _ _ s2 _) | s1 == s2 = Tuple (Match unit) Map.empty
  typeHeadsAreEqual t (TypeVar _ v) = Tuple (Match unit) (Map.singleton v [t])
  typeHeadsAreEqual (TypeConstructor _ c1) (TypeConstructor _ c2) | c1 == c2 = Tuple (Match unit) Map.empty
  typeHeadsAreEqual (TypeLevelString _ s1) (TypeLevelString _ s2) | s1 == s2 = Tuple (Match unit) Map.empty
  typeHeadsAreEqual (TypeLevelInt _ n1) (TypeLevelInt _ n2) | n1 == n2 = Tuple (Match unit) Map.empty
  typeHeadsAreEqual (TypeApp _ h1 t1) (TypeApp _ h2 t2) = bothMatch (typeHeadsAreEqual h1 h2) (typeHeadsAreEqual t1 t2)
  typeHeadsAreEqual (KindApp _ h1 t1) (KindApp _ h2 t2) = bothMatch (typeHeadsAreEqual h1 h2) (typeHeadsAreEqual t1 t2)
  typeHeadsAreEqual (REmpty _) (REmpty _) = Tuple (Match unit) Map.empty
  typeHeadsAreEqual r1@(RCons _ _ _ _) r2@(RCons _ _ _ _) = rowsHeadsAreEqual r1 r2
  typeHeadsAreEqual (REmpty _) r2@(RCons _ _ _ _) = rowsHeadsAreEqual (REmpty (getAnn r2)) r2
  typeHeadsAreEqual r1@(RCons _ _ _ _) (REmpty _) = rowsHeadsAreEqual r1 (REmpty (getAnn r1))
  typeHeadsAreEqual (TUnknown _ _) _ = Tuple Unknown Map.empty
  typeHeadsAreEqual (Skolem _ _ _ _ _) _ = Tuple Unknown Map.empty
  typeHeadsAreEqual _ _ = Tuple Apart Map.empty

  getAnn :: forall a. Type a -> a
  getAnn (TypeApp a _ _) = a
  getAnn (TypeConstructor a _) = a
  getAnn (REmpty a) = a
  getAnn (RCons a _ _ _) = a
  getAnn (TUnknown a _) = a
  getAnn (TypeVar a _) = a
  getAnn (TypeLevelString a _) = a
  getAnn (TypeLevelInt a _) = a
  getAnn (KindApp a _ _) = a
  getAnn (KindedType a _ _) = a
  getAnn (ForAll a _ _ _ _ _) = a
  getAnn (ConstrainedType a _ _) = a
  getAnn (Skolem a _ _ _ _) = a
  getAnn (TypeWildcard a _) = a
  getAnn (TypeOp a _) = a
  getAnn (BinaryNoParensType a _ _ _) = a
  getAnn (ParensInType a _) = a

  rowsHeadsAreEqual :: forall a. Eq a => Type a -> Type a -> Tuple (Matched Unit) (Matching (Array (Type a)))
  rowsHeadsAreEqual r1 r2 =
    let Tuple common rest = alignRowsWith (\_ -> typeHeadsAreEqual) r1 r2
        Tuple (Tuple leftRem leftTail) (Tuple rightRem rightTail) = rest
        restResult = goRows leftRem leftTail rightRem rightTail
    in foldl bothMatch restResult common

  goRows
    :: forall a
     . Eq a
    => Array (RowListItem a) -> Type a
    -> Array (RowListItem a) -> Type a
    -> Tuple (Matched Unit) (Matching (Array (Type a)))
  goRows l (KindedType _ t1 _) r t2 = goRows l t1 r t2
  goRows l t1 r (KindedType _ t2 _) = goRows l t1 r t2
  goRows l (KindApp _ t1 k1) r (KindApp _ t2 k2)
    | eqTypeSimple k1 k2 = goRows l t1 r t2
  goRows [] (REmpty _) [] (REmpty _) = Tuple (Match unit) Map.empty
  goRows [] (TUnknown _ u1) [] (TUnknown _ u2) | u1 == u2 = Tuple (Match unit) Map.empty
  goRows [] (TypeVar _ v1) [] (TypeVar _ v2) | v1 == v2 = Tuple (Match unit) Map.empty
  goRows [] (Skolem _ _ _ sk1 _) [] (Skolem _ _ _ sk2 _) | sk1 == sk2 = Tuple (Match unit) Map.empty
  goRows [] (TUnknown _ _) _ _ = Tuple Unknown Map.empty
  goRows sd r [] (TypeVar _ v) = Tuple (Match unit) (Map.singleton v [rowFromList (Tuple sd r)])
  goRows _ _ _ _ = Tuple Apart Map.empty

  eqTypeSimple :: forall a. Eq a => Type a -> Type a -> Boolean
  eqTypeSimple t1 t2 = t1 == t2

  bothMatch
    :: forall a
     . Eq a
    => Tuple (Matched Unit) (Matching (Array (Type a)))
    -> Tuple (Matched Unit) (Matching (Array (Type a)))
    -> Tuple (Matched Unit) (Matching (Array (Type a)))
  bothMatch (Tuple b1 m1) (Tuple b2 m2) = Tuple (b1 <> b2) (Map.unionWith (<>) m1 m2)

  verifySubstitution :: Matching (Array SourceType) -> Matched (Matching (Array SourceType))
  verifySubstitution mts =
    let valid = foldl (\acc types -> acc <> pairwiseAll typesAreEqual types) (Match unit) (Map.values mts)
    in case valid of
         Match _ -> Match mts
         Apart   -> Apart
         Unknown -> Unknown

  pairwiseAll :: forall t a. Monoid t => (a -> a -> t) -> Array a -> t
  pairwiseAll _ [] = mempty
  pairwiseAll _ [_] = mempty
  pairwiseAll p arr = case Array.uncons arr of
    Nothing -> mempty
    Just { head: x, tail: xs } -> Array.foldMap (p x) xs <> pairwiseAll p xs

  typesAreEqual :: SourceType -> SourceType -> Matched Unit
  typesAreEqual (KindedType _ t1 _) t2 = typesAreEqual t1 t2
  typesAreEqual t1 (KindedType _ t2 _) = typesAreEqual t1 t2
  typesAreEqual (TUnknown _ u1) (TUnknown _ u2) | u1 == u2 = Match unit
  typesAreEqual (TUnknown _ u1) t2 =
    if containsUnknownType u1 t2 then Apart else Unknown
  typesAreEqual t1 (TUnknown _ u2) =
    if containsUnknownType u2 t1 then Apart else Unknown
  typesAreEqual (Skolem _ _ _ s1 _) (Skolem _ _ _ s2 _) | s1 == s2 = Match unit
  typesAreEqual (Skolem _ _ _ s1 _) t2 =
    if containsSkolemType s1 t2 then Apart else Unknown
  typesAreEqual t1 (Skolem _ _ _ s2 _) =
    if containsSkolemType s2 t1 then Apart else Unknown
  typesAreEqual (TypeVar _ v1) (TypeVar _ v2) | v1 == v2 = Match unit
  typesAreEqual (TypeLevelString _ s1) (TypeLevelString _ s2) | s1 == s2 = Match unit
  typesAreEqual (TypeLevelInt _ n1) (TypeLevelInt _ n2) | n1 == n2 = Match unit
  typesAreEqual (TypeConstructor _ c1) (TypeConstructor _ c2) | c1 == c2 = Match unit
  typesAreEqual (TypeApp _ h1 t1) (TypeApp _ h2 t2) = typesAreEqual h1 h2 <> typesAreEqual t1 t2
  typesAreEqual (KindApp _ h1 t1) (KindApp _ h2 t2) = typesAreEqual h1 h2 <> typesAreEqual t1 t2
  typesAreEqual (REmpty _) (REmpty _) = Match unit
  typesAreEqual _ _ = Apart

  containsUnknownType :: Int -> SourceType -> Boolean
  containsUnknownType u = everythingOnTypes (||) (\ty -> case ty of
    TUnknown _ u' -> u == u'
    _             -> false)

  containsSkolemType :: Int -> SourceType -> Boolean
  containsSkolemType s = everythingOnTypes (||) (\ty -> case ty of
    Skolem _ _ _ s' _ -> s == s'
    _                 -> false)

-- | Process a chain of dictionaries to find matching instances
processChain
  :: Array TypeClassDict
  -> Array (Tuple (Maybe (Qualified (Either SourceType Ident))) (Maybe (Tuple (Matching (Array SourceType)) TypeClassDict)))
processChain = map (\d -> Tuple Nothing Nothing)
-- TODO: real implementation; for now returns empty

-- | Group dictionaries by chain
groupByChain :: Array TypeClassDict -> Array (Array TypeClassDict)
groupByChain dicts =
  map Array.fromFoldable <<< Array.fromFoldable <<<
  groupByEq (\(TypeClassDictionaryInScope d1) (TypeClassDictionaryInScope d2) -> d1.tcdChain == d2.tcdChain) $
  dicts

groupByEq :: forall a. (a -> a -> Boolean) -> Array a -> Array (NonEmptyList a)
groupByEq _ [] = []
groupByEq eq arr = case Array.uncons arr of
  Nothing -> []
  Just { head: x, tail: xs } ->
    let { init: same, rest } = Array.span (eq x) xs
        group = NEL.appendFoldable (NEL.singleton x) same
    in Array.cons group (groupByEq eq rest)

-- | Check any pair of values in a list match a predicate
pairwiseAny :: forall a. (a -> a -> Boolean) -> Array a -> Boolean
pairwiseAny _ [] = false
pairwiseAny _ [_] = false
pairwiseAny p arr = case Array.uncons arr of
  Nothing -> false
  Just { head: x, tail: xs } -> Array.any (p x) xs || pairwiseAny p xs

-- | Add a dictionary for the constraint to the scope, and dictionaries
-- | for all implied superclass instances.
newDictionaries
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => Array (Tuple (Qualified (ProperName ClassName)) Int)
  -> Qualified Ident
  -> SourceConstraint
  -> m (Array NamedDict)
newDictionaries path name constraint = do
  let Constraint con = constraint
      className = con.constraintClass
      instanceKinds = con.constraintKindArgs
      instanceTy = con.constraintArgs
  tcs <- gets \(CheckState s) -> let Environment env = s.checkEnv in env.typeClasses
  tcd <- case Map.lookup className tcs of
    Nothing -> internalCompilerError "newDictionaries: type class lookup failed"
    Just x -> pure x
  let TypeClassData tcData = tcd
  let sub = Array.zip (map fst tcData.typeClassArguments) instanceTy
  supDicts <- Array.concat <$> traverse
    (\(Tuple (Constraint supCon) index) ->
       newDictionaries
         (Array.snoc path (Tuple supCon.constraintClass index))
         name
         (Constraint supCon
           { constraintKindArgs = map (replaceAllTypeVars sub) supCon.constraintKindArgs
           , constraintArgs = map (replaceAllTypeVars sub) supCon.constraintArgs
           }))
    (Array.zip tcData.typeClassSuperclasses (Array.range 0 (Array.length tcData.typeClassSuperclasses - 1)))
  pure (Array.cons
    (TypeClassDictionaryInScope
      { tcdChain: Nothing
      , tcdIndex: 0
      , tcdValue: name
      , tcdPath: path
      , tcdClassName: className
      , tcdForAll: []
      , tcdInstanceKinds: instanceKinds
      , tcdInstanceTypes: instanceTy
      , tcdDependencies: Nothing
      , tcdDescription: Nothing
      })
    supDicts)

mkContext :: Array NamedDict -> InstanceContext
mkContext = foldr (\d acc ->
  let TypeClassDictionaryInScope dR = d
      Qualified qb _ = dR.tcdValue
      entry = Map.singleton qb (Map.singleton dR.tcdClassName (Map.singleton dR.tcdValue (NEL.singleton d)))
  in Map.unionWith (Map.unionWith (Map.unionWith (<>))) entry acc)
  Map.empty

-- | Exported entails function for external use
entails
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => MonadSupply m
  => SolverOptions
  -> SourceConstraint
  -> Map QualifiedBy (Map (Qualified (ProperName ClassName)) (Map (Qualified Ident) (Array NamedDict)))
  -> Array ErrorMessageHint
  -> m Expr
entails opts constraint context hints = do
  Tuple (Tuple result _) _ <- runStateT
    (runWriterT
      (entailsInner opts.solverShouldGeneralize opts.solverDeferErrors constraint context hints))
    Map.empty
  pure result

-- | Unsafe internal error helper for pure contexts
internalCompilerErrorPure :: forall a. String -> a
internalCompilerErrorPure _msg = Unsafe.Coerce.unsafeCoerce unit
