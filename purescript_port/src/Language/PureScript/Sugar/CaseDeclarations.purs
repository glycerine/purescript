module Language.PureScript.Sugar.CaseDeclarations
  ( desugarCases
  , desugarCasesModule
  , desugarCaseGuards
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Supply.Class (class MonadSupply, freshIdent')
import Data.Array as Array
import Data.Array (catMaybes, mapMaybe)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse, for)
import Data.Tuple (Tuple(..), fst, snd)

import Language.PureScript.AST.Binders (Binder(..), isIrrefutable)
import Language.PureScript.AST.Declarations
  ( CaseAlternative(..)
  , Declaration(..)
  , ErrorMessageHint(..)
  , Expr(..)
  , Guard(..)
  , GuardedExpr(..)
  , Module(..)
  , ValueDeclarationData(..)
  , WhereProvenance(..)
  , TypeInstanceBody(..)
  , declSourceSpan
  , traverseTypeInstanceBody
  )
import Language.PureScript.AST.Traversals (everywhereOnValuesM, everywhereOnValuesTopDownM)
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (SourcePos(..), SourceSpan(..), nullSourceSpan, nullSourceAnn)
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (NameKind(..))
import Language.PureScript.Errors
  ( ErrorMessage(..)
  , MultipleErrors(..)
  , SimpleErrorMessage(..)
  , addHint
  , errorMessage'
  , parU
  , rethrow
  , withPosition
  )
import Language.PureScript.Names (Ident(..), Qualified(..), QualifiedBy(..))

guardWith :: forall m e. MonadError e m => e -> Boolean -> m Unit
guardWith _ true = pure unit
guardWith e false = throwError e

desugarCasesModule
  :: forall m
   . MonadSupply m
  => MonadError MultipleErrors m
  => Module
  -> m Module
desugarCasesModule (Module ss coms name ds exps) =
  rethrow (addHint (ErrorInModule name)) $
    Module ss coms name
      <$> (desugarCases =<< desugarAbs =<< validateCases ds)
      <*> pure exps

desugarCaseGuards
  :: forall m
   . MonadSupply m
  => MonadError MultipleErrors m
  => Array Declaration
  -> m (Array Declaration)
desugarCaseGuards declarations = parU declarations go
  where
    go d =
      let t = everywhereOnValuesM pure (desugarGuardedExprs (declSourceSpan d)) pure
      in t.decl d

isTrivialExpr :: Expr -> Boolean
isTrivialExpr (Var _ _) = true
isTrivialExpr (Literal _ _) = true
isTrivialExpr (Accessor _ e) = isTrivialExpr e
isTrivialExpr (Parens e) = isTrivialExpr e
isTrivialExpr (PositionedValue _ _ e) = isTrivialExpr e
isTrivialExpr (TypedValue _ e _) = isTrivialExpr e
isTrivialExpr _ = false

desugarGuardedExprs
  :: forall m
   . MonadSupply m
  => SourceSpan
  -> Expr
  -> m Expr
desugarGuardedExprs ss (Case scrut alternatives)
  | not (Array.all isTrivialExpr scrut) = do
    pairs <- traverse (\e -> do
      scrutId <- freshIdent'
      let nullPos = SourcePos { line: 0, column: 0 }
          varExpr = Var ss (Qualified (BySourcePos nullPos) scrutId)
          decl = ValueDeclaration (ValueDeclarationData
            { valdeclSourceAnn: nullSourceAnn
            , valdeclIdent: scrutId
            , valdeclName: Private
            , valdeclBinders: []
            , valdeclExpression: [GuardedExpr [] e]
            })
      pure (Tuple varExpr decl)) scrut
    let scrut' = map fst pairs
        scrutDecls = map snd pairs
    inner <- desugarGuardedExprs ss (Case scrut' alternatives)
    pure $ Let FromLet scrutDecls inner

desugarGuardedExprs ss (Case scrut alternatives) =
  let
    desugarAlternatives :: Array CaseAlternative -> m (Array CaseAlternative)
    desugarAlternatives alts = case Array.uncons alts of
      Nothing -> pure []
      Just { head: a@(CaseAlternative ca), tail: rest }
        | [GuardedExpr [] _] <- ca.caseAlternativeResult ->
            map (Array.cons a) (desugarAlternatives rest)
      Just { head: CaseAlternative ca, tail: rest } ->
        let condGuards = Array.takeWhile isSingleCondGuard ca.caseAlternativeResult
            remaining  = Array.dropWhile isSingleCondGuard ca.caseAlternativeResult
        in if not (Array.null condGuards)
           then do
             tail' <- desugarGuardedAlternative ca.caseAlternativeBinders remaining rest
             pure $ Array.cons (CaseAlternative { caseAlternativeBinders: ca.caseAlternativeBinders, caseAlternativeResult: condGuards }) tail'
           else desugarGuardedAlternative ca.caseAlternativeBinders ca.caseAlternativeResult rest

    isSingleCondGuard (GuardedExpr [ConditionGuard _] _) = true
    isSingleCondGuard _ = false

    desugarGuardedAlternative
      :: Array Binder
      -> Array GuardedExpr
      -> Array CaseAlternative
      -> m (Array CaseAlternative)
    desugarGuardedAlternative _vb [] remAlts = desugarAlternatives remAlts
    desugarGuardedAlternative vb guardedExprs remAlts =
      case Array.uncons guardedExprs of
        Just { head: GuardedExpr gs e, tail: ge } -> do
          rhs <- desugarAltOutOfLine vb ge remAlts $ \altFail ->
            let altFail' n = if Array.all isIrrefutable vb then [] else altFail n
            in Case scrut
                (Array.cons
                  (CaseAlternative { caseAlternativeBinders: vb, caseAlternativeResult: [GuardedExpr [] (desugarGuard gs e altFail)] })
                  (altFail' (Array.length scrut)))
          pure [CaseAlternative { caseAlternativeBinders: scrutNullBinder, caseAlternativeResult: [GuardedExpr [] rhs] }]
        _ -> pure []

    desugarGuard :: Array Guard -> Expr -> (Int -> Array CaseAlternative) -> Expr
    desugarGuard [] e _ = e
    desugarGuard guards e matchFailed = case Array.uncons guards of
      Nothing -> e
      Just { head: ConditionGuard c, tail: gs }
        | isTrueExpr c -> desugarGuard gs e matchFailed
        | otherwise ->
            Case [c]
              (Array.cons
                (CaseAlternative { caseAlternativeBinders: [LiteralBinder ss (BooleanLiteral true)], caseAlternativeResult: [GuardedExpr [] (desugarGuard gs e matchFailed)] })
                (matchFailed 1))
      Just { head: PatternGuard vb g, tail: gs } ->
        Case [g]
          (Array.cons
            (CaseAlternative { caseAlternativeBinders: [vb], caseAlternativeResult: [GuardedExpr [] (desugarGuard gs e matchFailed)] })
            (if isIrrefutable vb then [] else matchFailed 1))

    isTrueExpr :: Expr -> Boolean
    isTrueExpr (Literal _ (BooleanLiteral true)) = true
    isTrueExpr (TypedValue _ e _) = isTrueExpr e
    isTrueExpr (PositionedValue _ _ e) = isTrueExpr e
    isTrueExpr _ = false

    desugarAltOutOfLine
      :: Array Binder
      -> Array GuardedExpr
      -> Array CaseAlternative
      -> ((Int -> Array CaseAlternative) -> Expr)
      -> m Expr
    desugarAltOutOfLine altBinder remGuarded remAlts mkBody =
      case mkCaseOfRemainingGuardsAndAlts of
        Just remCase -> do
          desugared   <- desugarGuardedExprs ss remCase
          remCaseId   <- freshIdent'
          unusedBinder <- freshIdent'
          let nullPos = SourcePos { line: 0, column: 0 }
              gotoRemCase = App (Var ss (Qualified (BySourcePos nullPos) remCaseId)) (Literal ss (BooleanLiteral true))
              altFail n = [CaseAlternative { caseAlternativeBinders: Array.replicate n NullBinder, caseAlternativeResult: [GuardedExpr [] gotoRemCase] }]
          pure $ Let FromLet
            [ ValueDeclaration (ValueDeclarationData
                { valdeclSourceAnn: nullSourceAnn
                , valdeclIdent: remCaseId
                , valdeclName: Private
                , valdeclBinders: []
                , valdeclExpression: [GuardedExpr [] (Abs (VarBinder ss unusedBinder) desugared)]
                })
            ] (mkBody altFail)
        Nothing ->
          pure $ mkBody (const [])
      where
        mkCaseOfRemainingGuardsAndAlts
          | not (Array.null remGuarded) =
              Just $ Case scrut (Array.cons (CaseAlternative { caseAlternativeBinders: altBinder, caseAlternativeResult: remGuarded }) remAlts)
          | not (Array.null remAlts) =
              Just $ Case scrut remAlts
          | otherwise = Nothing

    scrutNullBinder :: Array Binder
    scrutNullBinder = Array.replicate (Array.length scrut) NullBinder

    isNullBinder :: Binder -> Boolean
    isNullBinder NullBinder = true
    isNullBinder (PositionedBinder _ _ b) = isNullBinder b
    isNullBinder (TypedBinder _ b) = isNullBinder b
    isNullBinder _ = false

    optimize :: Expr -> Expr
    optimize (Case _ [CaseAlternative ca])
      | [GuardedExpr [] v] <- ca.caseAlternativeResult
      , Array.all isNullBinder ca.caseAlternativeBinders = v
    optimize e = e
  in do
    alts' <- desugarAlternatives alternatives
    pure $ optimize (Case scrut alts')

desugarGuardedExprs ss (TypedValue inferred e ty) =
  TypedValue inferred <$> desugarGuardedExprs ss e <*> pure ty

desugarGuardedExprs _ (PositionedValue ss comms e) =
  PositionedValue ss comms <$> desugarGuardedExprs ss e

desugarGuardedExprs _ v = pure v

validateCases :: forall m. MonadSupply m => MonadError MultipleErrors m => Array Declaration -> m (Array Declaration)
validateCases = flip parU f
  where
  t = everywhereOnValuesM pure validate pure
  f = t.decl

  validate :: Expr -> m Expr
  validate c@(Case vs alts) = do
    let l = Array.length vs
        badAlts = Array.filter (\(CaseAlternative ca) -> l /= Array.length ca.caseAlternativeBinders) alts
    if Array.null badAlts
      then pure c
      else throwError $ MultipleErrors (map (altError l) (map (\(CaseAlternative ca) -> ca.caseAlternativeBinders) badAlts))
  validate other = pure other

  altError :: Int -> Array Binder -> ErrorMessage
  altError l bs = withPosition pos $ ErrorMessage [] $ CaseBinderLengthDiffers l bs
    where
    pos = fromMaybe nullSourceSpan (foldl1May widenSpan (mapMaybe positionedBinder bs))

    widenSpan (SourceSpan a) (SourceSpan b) =
      SourceSpan
        { name: a.name
        , start: if a.start <= b.start then a.start else b.start
        , end:   if a.end >= b.end then a.end else b.end
        }

    positionedBinder (PositionedBinder p _ _) = Just p
    positionedBinder _ = Nothing

foldl1May :: forall a. (a -> a -> a) -> Array a -> Maybe a
foldl1May f arr = case Array.uncons arr of
  Nothing -> Nothing
  Just { head: x, tail: xs } -> Just (Array.foldl f x xs)

desugarAbs :: forall m. MonadSupply m => MonadError MultipleErrors m => Array Declaration -> m (Array Declaration)
desugarAbs = flip parU f
  where
  t = everywhereOnValuesM pure replace pure
  f = t.decl

  replace :: Expr -> m Expr
  replace (Abs binder val) =
    case stripPositioned binder of
      VarBinder ss i -> pure (Abs (VarBinder ss i) val)
      strippedBinder -> do
        ident <- freshIdent'
        let nullPos = SourcePos { line: 0, column: 0 }
        pure $ Abs (VarBinder nullSourceSpan ident) $
          Case [Var nullSourceSpan (Qualified (BySourcePos nullPos) ident)]
            [CaseAlternative { caseAlternativeBinders: [strippedBinder], caseAlternativeResult: [GuardedExpr [] val] }]
  replace other = pure other

stripPositioned :: Binder -> Binder
stripPositioned (PositionedBinder _ _ binder) = stripPositioned binder
stripPositioned binder = binder

desugarCases :: forall m. MonadSupply m => MonadError MultipleErrors m => Array Declaration -> m (Array Declaration)
desugarCases ds = do
  grouped <- map Array.concat (traverse toDecls (groupByInSameGroup ds))
  desugarRest grouped
  where
    desugarRest :: Array Declaration -> m (Array Declaration)
    desugarRest decls = case Array.uncons decls of
      Nothing -> pure []
      Just { head: TypeInstanceDeclaration sa na cd idx name constraints className tys body, tail: rest } -> do
        body' <- traverseTypeInstanceBody desugarCases body
        rest' <- desugarRest rest
        pure $ Array.cons (TypeInstanceDeclaration sa na cd idx name constraints className tys body') rest'
      Just { head: ValueDeclaration (ValueDeclarationData vd), tail: rest } ->
        let t = everywhereOnValuesTopDownM pure go pure
            f' = traverse (\(GuardedExpr gs e) -> GuardedExpr gs <$> t.expr e)
        in do
          result' <- f' vd.valdeclExpression
          rest' <- desugarRest rest
          pure $ Array.cons (ValueDeclaration (ValueDeclarationData vd { valdeclExpression = result' })) rest'
        where
        go (Let w ds' val') = Let w <$> desugarCases ds' <*> pure val'
        go other = pure other
      Just { head: d, tail: rest } -> do
        rest' <- desugarRest rest
        pure $ Array.cons d rest'

groupByInSameGroup :: Array Declaration -> Array (Array Declaration)
groupByInSameGroup = go []
  where
  go acc [] = if Array.null acc then [] else [Array.reverse acc]
  go acc arr = case Array.uncons arr of
    Nothing -> if Array.null acc then [] else [Array.reverse acc]
    Just { head: x, tail: xs } -> case Array.head acc of
      Nothing -> go [x] xs
      Just prev ->
        if inSameGroup x prev
          then go (Array.cons x acc) xs
          else Array.cons (Array.reverse acc) (go [x] xs)

inSameGroup :: Declaration -> Declaration -> Boolean
inSameGroup (ValueDeclaration (ValueDeclarationData vd1)) (ValueDeclaration (ValueDeclarationData vd2)) =
  vd1.valdeclIdent == vd2.valdeclIdent
inSameGroup _ _ = false

toDecls :: forall m. MonadSupply m => MonadError MultipleErrors m => Array Declaration -> m (Array Declaration)
toDecls ds = case ds of
  [ValueDeclaration (ValueDeclarationData vd)]
    | [GuardedExpr [] _] <- vd.valdeclExpression
    , Array.all isIrrefutable vd.valdeclBinders -> do
        let ss = fst vd.valdeclSourceAnn
        args <- traverse fromVarBinder vd.valdeclBinders
        let body = Array.foldr (Abs <<< VarBinder ss) (case Array.head vd.valdeclExpression of
                     Just (GuardedExpr [] val) -> val
                     _ -> internalError "toDecls: impossible") args
        guardWith (errorMessage' ss (OverlappingArgNames (Just vd.valdeclIdent)))
          (Array.length (ordNub args) == Array.length args)
        pure [ValueDeclaration (ValueDeclarationData vd { valdeclBinders = [], valdeclExpression = [GuardedExpr [] body] })]
  [] -> pure []
  _ -> case Array.head ds of
    Just (ValueDeclaration (ValueDeclarationData vd)) -> do
      let ss = fst vd.valdeclSourceAnn
          tuples = mapMaybe toTuple ds
          result = fromMaybe [] (map snd (Array.head tuples))
          bsLen = fromMaybe 0 (map (Array.length <<< fst) (Array.head tuples))

          isGuarded (GuardedExpr [] _) = false
          isGuarded _ = true

      if not (Array.all (\t -> Array.length (fst t) == bsLen) tuples)
        then throwError (errorMessage' ss (ArgListLengthsDiffer vd.valdeclIdent))
        else if Array.null vd.valdeclBinders && not (Array.any isGuarded result)
          then throwError (errorMessage' ss (DuplicateValueDeclaration vd.valdeclIdent))
          else do
            caseDecl <- makeCaseDeclaration ss vd.valdeclIdent tuples
            pure [caseDecl]
    _ -> pure ds
  where
  fromVarBinder :: Binder -> m Ident
  fromVarBinder NullBinder = freshIdent'
  fromVarBinder (VarBinder _ name) = pure name
  fromVarBinder (PositionedBinder _ _ b) = fromVarBinder b
  fromVarBinder (TypedBinder _ b) = fromVarBinder b
  fromVarBinder _ = internalError "fromVarBinder: Invalid argument"

toTuple :: Declaration -> Maybe (Tuple (Array Binder) (Array GuardedExpr))
toTuple (ValueDeclaration (ValueDeclarationData vd)) = Just (Tuple vd.valdeclBinders vd.valdeclExpression)
toTuple _ = Nothing

makeCaseDeclaration
  :: forall m
   . MonadSupply m
  => SourceSpan
  -> Ident
  -> Array (Tuple (Array Binder) (Array GuardedExpr))
  -> m Declaration
makeCaseDeclaration ss ident alternatives = do
  let namedArgs = map (map findName <<< fst) alternatives
      argNames = case Array.head namedArgs of
        Nothing -> []
        Just first -> Array.foldl resolveNames first (Array.drop 1 namedArgs)
  args <- if allUnique (catMaybes argNames)
            then traverse argName argNames
            else replicateM (fromMaybe 0 (map Array.length (Array.head namedArgs))) freshArg
  let nullPos = SourcePos { line: 0, column: 0 }
      vars = map (\(Tuple argSs i) -> Var ss (Qualified (BySourcePos nullPos) i)) args
      binders = map (\(Tuple bs result) -> CaseAlternative { caseAlternativeBinders: bs, caseAlternativeResult: result }) alternatives
      value = Array.foldr (\(Tuple argSs i) acc -> Abs (VarBinder argSs i) acc) (Case vars binders) args
  pure $ ValueDeclaration (ValueDeclarationData
    { valdeclSourceAnn: Tuple ss []
    , valdeclIdent: ident
    , valdeclName: Public
    , valdeclBinders: []
    , valdeclExpression: [GuardedExpr [] value]
    })
  where
  findName :: Binder -> Maybe (Tuple SourceSpan Ident)
  findName (VarBinder ss' name) = Just (Tuple ss' name)
  findName (PositionedBinder _ _ binder) = findName binder
  findName _ = Nothing

  allUnique :: forall a. Eq a => Ord a => Array a -> Boolean
  allUnique xs = Array.length xs == Array.length (ordNub xs)

  argName :: Maybe (Tuple SourceSpan Ident) -> m (Tuple SourceSpan Ident)
  argName (Just p) = pure p
  argName Nothing = freshArg

  freshArg :: m (Tuple SourceSpan Ident)
  freshArg = map (Tuple nullSourceSpan) freshIdent'

  resolveNames
    :: Array (Maybe (Tuple SourceSpan Ident))
    -> Array (Maybe (Tuple SourceSpan Ident))
    -> Array (Maybe (Tuple SourceSpan Ident))
  resolveNames = Array.zipWith resolveName

  resolveName
    :: Maybe (Tuple SourceSpan Ident)
    -> Maybe (Tuple SourceSpan Ident)
    -> Maybe (Tuple SourceSpan Ident)
  resolveName (Just (Tuple _ a)) (Just (Tuple _ b))
    | a == b = Just (Tuple nullSourceSpan a)
    | otherwise = Nothing
  resolveName _ _ = Nothing

ordNub :: forall a. Ord a => Array a -> Array a
ordNub = Array.nub

replicateM :: forall m a. Monad m => Int -> m a -> m (Array a)
replicateM n m
  | n <= 0 = pure []
  | otherwise = do
      x <- m
      xs <- replicateM (n - 1) m
      pure (Array.cons x xs)
