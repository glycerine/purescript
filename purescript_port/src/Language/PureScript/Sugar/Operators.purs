module Language.PureScript.Sugar.Operators
  ( desugarSignedLiterals
  , RebracketCaller(..)
  , rebracket
  , rebracketFiltered
  , checkFixityExports
  ) where

import Prelude

import Control.Monad ((>=>))
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Supply.Class (class MonadSupply)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Identity (Identity(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Array (head) as Array
import Data.Maybe (Maybe(..), fromMaybe) as M
import Data.Maybe (Maybe(..))
import Data.Traversable (for_, traverse, traverse_)
import Data.Tuple (Tuple(..), fst, snd)

import Language.PureScript.AST.Binders (Binder(..))
import Language.PureScript.AST.Declarations
  ( Declaration(..)
  , DeclarationRef(..)
  , ErrorMessageHint(..)
  , Expr(..)
  , Module(..)
  , TypeDeclarationData(..)
  , TypeFixity(..)
  , ValueFixity(..)
  , declSourceSpan
  , getFixityDecl
  , getTypeOpRef
  , getValueOpRef
  , isAnonymousArgument
  , traverseDataCtorFields
  )
import Language.PureScript.AST.Operators (Associativity, Fixity(..), Precedence)
import Language.PureScript.AST.SourcePos (SourceAnn, SourceSpan, internalModuleSourceSpan, nullSourceSpan)
import Language.PureScript.AST.Traversals
  ( defS
  , everywhereOnValues
  , everywhereOnValuesTopDownM
  , everywhereWithContextOnValuesM
  , sndM
  )
import Language.PureScript.Crash (internalError)
import Language.PureScript.Errors
  ( MultipleErrors
  , SimpleErrorMessage(..)
  , addHint
  , errorMessage
  , errorMessage'
  , parU
  , rethrow
  , rethrowWithPosition
  )
import Language.PureScript.Externs (ExternsFile(..), ExternsFixity(..), ExternsTypeFixity(..))
import Language.PureScript.Names
  ( ConstructorName
  , Ident(..)
  , ModuleName
  , Name(..)
  , OpName
  , ProperName
  , Qualified(..)
  , QualifiedBy(..)
  , TypeName
  , TypeOpName
  , ValueOpName
  , byNullSourcePos
  )
import Control.Monad.Supply (freshIdent')
import Language.PureScript.Sugar.Operators.Binders (matchBinderOperators)
import Language.PureScript.Sugar.Operators.Expr (matchExprOperators)
import Language.PureScript.Sugar.Operators.Types (matchTypeOperators)
import Language.PureScript.Types
  ( Constraint(..)
  , SourceType
  , Type(..)
  , everywhereOnTypesTopDownM
  , overConstraintArgs
  )
import Language.PureScript.Constants.Libs as C

desugarSignedLiterals :: Module -> Module
desugarSignedLiterals (Module ss coms mn ds exts) =
  Module ss coms mn (map f' ds) exts
  where
  t = everywhereOnValues identity go identity
  f' = t.decl

  go :: Expr -> Expr
  go (UnaryMinus ss' val) = App (Var ss' (Qualified byNullSourcePos (Ident C.sNegate))) val
  go other = other

type FixityRecord op alias =
  { qualified :: Qualified op
  , pos       :: SourceSpan
  , fixity    :: Fixity
  , alias     :: Qualified alias
  }
type ValueFixityRecord = FixityRecord (OpName ValueOpName) (Either Ident (ProperName ConstructorName))
type TypeFixityRecord  = FixityRecord (OpName TypeOpName)  (ProperName TypeName)

data RebracketCaller
  = CalledByCompile
  | CalledByDocs

derive instance eqRebracketCaller :: Eq RebracketCaller

instance showRebracketCaller :: Show RebracketCaller where
  show CalledByCompile = "CalledByCompile"
  show CalledByDocs    = "CalledByDocs"

rebracket
  :: forall m
   . MonadError MultipleErrors m
  => MonadSupply m
  => Array ExternsFile
  -> Module
  -> m Module
rebracket = rebracketFiltered CalledByCompile (const true)

rebracketFiltered
  :: forall m
   . MonadError MultipleErrors m
  => MonadSupply m
  => RebracketCaller
  -> (Declaration -> Boolean)
  -> Array ExternsFile
  -> Module
  -> m Module
rebracketFiltered caller pred_ externs m = do
  let allFixities = Array.concatMap externsFixities externs <> collectFixities m
      { left: valueFixities, right: typeFixities } = partitionEithers' allFixities

  ensureNoDuplicates' MultipleValueOpFixities valueFixities
  ensureNoDuplicates' MultipleTypeOpFixities typeFixities

  let valueOpTable = customOperatorTable' valueFixities
      valueAliased = Map.fromFoldable (map makeLookupEntry valueFixities)
      typeOpTable  = customOperatorTable' typeFixities
      typeAliased  = Map.fromFoldable (map makeLookupEntry typeFixities)

  rebracketModule caller pred_ valueOpTable typeOpTable m >>=
    renameAliasedOperators valueAliased typeAliased

  where

  partitionEithers' :: forall a b. Array (Either a b) -> { left :: Array a, right :: Array b }
  partitionEithers' = Array.foldl
    (\acc x -> case x of
      Left  a -> acc { left  = Array.snoc acc.left  a }
      Right b -> acc { right = Array.snoc acc.right b })
    { left: [], right: [] }

  ensureNoDuplicates'
    :: forall op alias
     . Ord op
    => (op -> SimpleErrorMessage)
    -> Array (FixityRecord op alias)
    -> m Unit
  ensureNoDuplicates' toError recs =
    ensureNoDuplicates toError (map (\r -> Tuple r.qualified r.pos) recs)

  customOperatorTable' :: forall op alias. Array (FixityRecord op alias) -> Array (Array (Tuple (Qualified op) Associativity))
  customOperatorTable' = customOperatorTable <<< map (\r -> Tuple r.qualified r.fixity)

  makeLookupEntry :: forall op alias. FixityRecord op alias -> Tuple (Qualified op) (Qualified alias)
  makeLookupEntry r = Tuple r.qualified r.alias

  renameAliasedOperators
    :: Map (Qualified (OpName ValueOpName)) (Qualified (Either Ident (ProperName ConstructorName)))
    -> Map (Qualified (OpName TypeOpName)) (Qualified (ProperName TypeName))
    -> Module
    -> m Module
  renameAliasedOperators valueAliased typeAliased (Module ss coms mn ds exts) =
    Module ss coms mn <$> traverse (usingPredicate pred_ f') ds <*> pure exts
    where
    goType :: SourceSpan -> SourceType -> m SourceType
    goType pos (TypeOp ann2 op) =
      case Map.lookup op typeAliased of
        Just alias -> pure $ TypeConstructor ann2 alias
        Nothing    -> throwError (errorMessage' pos (UnknownName (map TyOpName op)))
    goType _ other = pure other

    { decl: goDecl', expr: goExpr', binder: goBinder' } = updateTypes goType

    f' :: Declaration -> m Declaration
    f' decl = do
      let ctx = everywhereWithContextOnValuesM
                  ss
                  (\_ d -> map (Tuple (declSourceSpan d)) (goDecl' d))
                  (\pos e -> do
                    Tuple pos' e' <- goExpr' pos e
                    e'' <- goExprAlias e'
                    pure (Tuple pos' e''))
                  (\pos b -> do
                    Tuple pos' b' <- goBinder' pos b
                    b'' <- goBinderAlias b'
                    pure (Tuple pos' b''))
                  defS
                  defS
                  defS
      ctx.decl decl

    goExprAlias :: Expr -> m Expr
    goExprAlias (Op pos op) =
      case Map.lookup op valueAliased of
        Just (Qualified mn' (Left alias)) ->
          pure $ Var pos (Qualified mn' alias)
        Just (Qualified mn' (Right alias)) ->
          pure $ Constructor pos (Qualified mn' alias)
        Nothing ->
          throwError (errorMessage' pos (UnknownName (map ValOpName op)))
    goExprAlias other = pure other

    goBinderAlias :: Binder -> m Binder
    goBinderAlias (BinaryNoParensBinder (OpBinder pos op) lhs rhs) =
      case Map.lookup op valueAliased of
        Just (Qualified mn' (Left alias)) ->
          throwError (errorMessage' pos (InvalidOperatorInBinder op (Qualified mn' alias)))
        Just (Qualified mn' (Right alias)) ->
          pure (ConstructorBinder pos (Qualified mn' alias) [lhs, rhs])
        Nothing ->
          throwError (errorMessage' pos (UnknownName (map ValOpName op)))
    goBinderAlias (BinaryNoParensBinder _ _ _) =
      internalError "BinaryNoParensBinder has no OpBinder"
    goBinderAlias other = pure other

rebracketModule
  :: forall m
   . MonadError MultipleErrors m
  => MonadSupply m
  => RebracketCaller
  -> (Declaration -> Boolean)
  -> Array (Array (Tuple (Qualified (OpName ValueOpName)) Associativity))
  -> Array (Array (Tuple (Qualified (OpName TypeOpName)) Associativity))
  -> Module
  -> m Module
rebracketModule caller pred_ valueOpTable typeOpTable (Module ss coms mn ds exts) =
  Module ss coms mn <$> f' ds <*> pure exts
  where
  goType :: SourceSpan -> SourceType -> m SourceType
  goType ss' ty = matchTypeOperators ss' typeOpTable ty

  { decl: goDecl, expr: goExpr', binder: goBinder' } = updateTypes goType

  f :: Declaration -> m Declaration
  f decl = do
    let ctx = everywhereWithContextOnValuesM
                ss
                (\_ d -> map (Tuple (declSourceSpan d)) (goDecl d))
                (\pos e -> do
                  Tuple pos' e' <- goExpr' pos e
                  e'' <- matchExprOperators valueOpTable e'
                  pure (Tuple pos' e''))
                (\pos b -> do
                  Tuple pos' b' <- goBinder' pos b
                  b'' <- matchBinderOperators valueOpTable b'
                  pure (Tuple pos' b''))
                defS
                defS
                defS
    ctx.decl decl

  g :: Declaration -> m Declaration
  g decl = do
    let ctx = everywhereOnValuesTopDownM pure removeBinaryNoParens pure
    ctx.decl decl

  h :: Declaration -> m Declaration
  h = case caller of
    CalledByDocs    -> f
    CalledByCompile -> g >=> f

  f' :: Array Declaration -> m (Array Declaration)
  f' decls = do
    processed <- parU decls (usingPredicate pred_ h)
    pure (map (\d -> if pred_ d then removeParens d else d) processed)

removeBinaryNoParens :: forall m. MonadError MultipleErrors m => MonadSupply m => Expr -> m Expr
removeBinaryNoParens u
  | isAnonymousArgument u =
      case u of
        PositionedValue p _ _ -> rethrowWithPosition p err
        _                     -> err
      where err = throwError (errorMessage IncorrectAnonymousArgument)
removeBinaryNoParens (Parens inner) = do
  let stripped = stripPositionInfo inner
  case stripped of
    BinaryNoParens op l r
      | isAnonymousArgument r -> do
          arg <- freshIdent'
          pure $ Abs (VarBinder nullSourceSpan arg)
               $ App (App op l) (Var nullSourceSpan (Qualified byNullSourcePos arg))
      | isAnonymousArgument l -> do
          arg <- freshIdent'
          pure $ Abs (VarBinder nullSourceSpan arg)
               $ App (App op (Var nullSourceSpan (Qualified byNullSourcePos arg))) r
    _ -> pure (Parens inner)
removeBinaryNoParens (BinaryNoParens op l r) = pure $ App (App op l) r
removeBinaryNoParens e = pure e

stripPositionInfo :: Expr -> Expr
stripPositionInfo (PositionedValue _ _ e) = stripPositionInfo e
stripPositionInfo e = e

removeParens :: Declaration -> Declaration
removeParens decl =
  let { decl: goDecl, expr: goExpr', binder: goBinder' } = updateTypes (\_ -> Identity <<< goType)
      t = everywhereOnValues
            (\d -> let Identity d' = goDecl d in d')
            (\e -> goExpr (let Identity (Tuple _ e') = goExpr' dummySS e in e'))
            (\b -> goBinder (let Identity (Tuple _ b') = goBinder' dummySS b in b'))
  in t.decl decl
  where
  dummySS :: SourceSpan
  dummySS = nullSourceSpan

  goExpr :: Expr -> Expr
  goExpr (Parens val) = goExpr val
  goExpr val = val

  goBinder :: Binder -> Binder
  goBinder (ParensInBinder b) = goBinder b
  goBinder b = b

  goType :: forall a. Type a -> Type a
  goType (ParensInType _ t) = goType t
  goType t = t

externsFixities :: ExternsFile -> Array (Either ValueFixityRecord TypeFixityRecord)
externsFixities (ExternsFile ef) =
  map fromFixity ef.efFixities <> map fromTypeFixity ef.efTypeFixities
  where
  fromFixity :: ExternsFixity -> Either ValueFixityRecord TypeFixityRecord
  fromFixity (ExternsFixity fix) = Left
    { qualified: Qualified (ByModuleName ef.efModuleName) fix.efOperator
    , pos: internalModuleSourceSpan ""
    , fixity: Fixity fix.efAssociativity fix.efPrecedence
    , alias: fix.efAlias
    }

  fromTypeFixity :: ExternsTypeFixity -> Either ValueFixityRecord TypeFixityRecord
  fromTypeFixity (ExternsTypeFixity fix) = Right
    { qualified: Qualified (ByModuleName ef.efModuleName) fix.efTypeOperator
    , pos: internalModuleSourceSpan ""
    , fixity: Fixity fix.efTypeAssociativity fix.efTypePrecedence
    , alias: fix.efTypeAlias
    }

collectFixities :: Module -> Array (Either ValueFixityRecord TypeFixityRecord)
collectFixities (Module _ _ moduleName ds _) = Array.concatMap collect ds
  where
  collect :: Declaration -> Array (Either ValueFixityRecord TypeFixityRecord)
  collect (FixityDeclaration (Tuple ss _) (Left (ValueFixity fixity name op))) =
    [ Left { qualified: Qualified (ByModuleName moduleName) op, pos: ss, fixity, alias: name } ]
  collect (FixityDeclaration (Tuple ss _) (Right (TypeFixity fixity name op))) =
    [ Right { qualified: Qualified (ByModuleName moduleName) op, pos: ss, fixity, alias: name } ]
  collect _ = []

ensureNoDuplicates
  :: forall m a
   . Ord a
  => MonadError MultipleErrors m
  => (a -> SimpleErrorMessage)
  -> Array (Tuple (Qualified a) SourceSpan)
  -> m Unit
ensureNoDuplicates toError entries = go (Array.sortBy (\(Tuple x _) (Tuple y _) -> compare x y) entries)
  where
  go :: Array (Tuple (Qualified a) SourceSpan) -> m Unit
  go arr = case Array.uncons arr of
    Nothing -> pure unit
    Just { head: _, tail: [] } -> pure unit
    Just { head: Tuple x _, tail } ->
      case Array.uncons tail of
        Nothing -> pure unit
        Just { head: Tuple y pos, tail: _ } ->
          if x == y
            then case x of
              Qualified (ByModuleName mn) op ->
                rethrow (addHint (ErrorInModule mn))
                  (rethrowWithPosition pos (throwError (errorMessage (toError op))))
              _ -> go tail
            else go tail

customOperatorTable
  :: forall op
   . Array (Tuple (Qualified op) Fixity)
  -> Array (Array (Tuple (Qualified op) Associativity))
customOperatorTable fixities =
  let userOps = map (\(Tuple name (Fixity a p)) -> { name, prec: p, assoc: a }) fixities
      sorted   = Array.sortBy (\a b -> compare b.prec a.prec) userOps
      grouped  = groupByPrec sorted
  in map (map (\r -> Tuple r.name r.assoc)) grouped
  where
  groupByPrec :: Array { name :: Qualified op, prec :: Precedence, assoc :: Associativity }
              -> Array (Array { name :: Qualified op, prec :: Precedence, assoc :: Associativity })
  groupByPrec [] = []
  groupByPrec arr = case Array.uncons arr of
    Nothing -> []
    Just { head: x, tail: xs } ->
      let same = Array.takeWhile (\y -> y.prec == x.prec) xs
          rest = Array.dropWhile (\y -> y.prec == x.prec) xs
      in Array.cons (Array.cons x same) (groupByPrec rest)

updateTypes
  :: forall m
   . Monad m
  => (SourceSpan -> SourceType -> m SourceType)
  -> { decl   :: Declaration -> m Declaration
     , expr   :: SourceSpan -> Expr -> m (Tuple SourceSpan Expr)
     , binder :: SourceSpan -> Binder -> m (Tuple SourceSpan Binder)
     }
updateTypes goType = { decl: goDecl, expr: goExpr, binder: goBinder }
  where

  goType' :: SourceSpan -> SourceType -> m SourceType
  goType' ss ty = everywhereOnTypesTopDownM (goType ss) ty

  goDecl :: Declaration -> m Declaration
  goDecl (DataDeclaration sa ddt name args dctors) =
    let ss = fst sa
    in DataDeclaration sa ddt name
      <$> traverse (traverse (traverse (goType' ss))) args
      <*> traverse (traverseDataCtorFields (traverse (sndM (goType' ss)))) dctors
  goDecl (ExternDeclaration sa name ty) =
    ExternDeclaration sa name <$> goType' (fst sa) ty
  goDecl (TypeClassDeclaration sa name args implies deps decls) = do
    let ss = fst sa
    implies' <- traverse (overConstraintArgs (traverse (goType' ss))) implies
    args'    <- traverse (traverse (traverse (goType' ss))) args
    pure $ TypeClassDeclaration sa name args' implies' deps decls
  goDecl (TypeInstanceDeclaration sa na ch idx name cs className tys impls) = do
    let ss = fst sa
    cs'  <- traverse (overConstraintArgs (traverse (goType' ss))) cs
    tys' <- traverse (goType' ss) tys
    pure $ TypeInstanceDeclaration sa na ch idx name cs' className tys' impls
  goDecl (TypeSynonymDeclaration sa name args ty) =
    TypeSynonymDeclaration sa name
      <$> traverse (traverse (traverse (goType' (fst sa)))) args
      <*> goType' (fst sa) ty
  goDecl (TypeDeclaration (TypeDeclarationData td)) =
    (\ty' -> TypeDeclaration (TypeDeclarationData td { tydeclType = ty' }))
      <$> goType' (fst td.tydeclSourceAnn) td.tydeclType
  goDecl (KindDeclaration sa sigFor name ty) =
    KindDeclaration sa sigFor name <$> goType' (fst sa) ty
  goDecl (ExternDataDeclaration sa name ty) =
    ExternDataDeclaration sa name <$> goType' (fst sa) ty
  goDecl other = pure other

  goExpr :: SourceSpan -> Expr -> m (Tuple SourceSpan Expr)
  goExpr _ e@(PositionedValue pos _ _) = pure (Tuple pos e)
  goExpr pos (TypeClassDictionary (Constraint c) dicts hints) = do
    kinds' <- traverse (goType' pos) c.constraintKindArgs
    tys'   <- traverse (goType' pos) c.constraintArgs
    pure (Tuple pos (TypeClassDictionary (Constraint c { constraintKindArgs = kinds', constraintArgs = tys' }) dicts hints))
  goExpr pos (DeferredDictionary cls tys) = do
    tys' <- traverse (goType' pos) tys
    pure (Tuple pos (DeferredDictionary cls tys'))
  goExpr pos (TypedValue check v ty) = do
    ty' <- goType' pos ty
    pure (Tuple pos (TypedValue check v ty'))
  goExpr pos (VisibleTypeApp v ty) = do
    ty' <- goType' pos ty
    pure (Tuple pos (VisibleTypeApp v ty'))
  goExpr pos other = pure (Tuple pos other)

  goBinder :: SourceSpan -> Binder -> m (Tuple SourceSpan Binder)
  goBinder _ e@(PositionedBinder pos _ _) = pure (Tuple pos e)
  goBinder pos (TypedBinder ty b) = do
    ty' <- goType' pos ty
    pure (Tuple pos (TypedBinder ty' b))
  goBinder pos other = pure (Tuple pos other)

checkFixityExports
  :: forall m
   . MonadError MultipleErrors m
  => Module
  -> m Module
checkFixityExports (Module _ _ _ _ Nothing) =
  internalError "exports should have been elaborated before checkFixityExports"
checkFixityExports m@(Module ss _ mn ds (Just exps)) =
  rethrow (addHint (ErrorInModule mn))
    $ rethrowWithPosition ss (traverse_ checkRef exps)
    $> m
  where

  checkRef :: DeclarationRef -> m Unit
  checkRef dr@(ValueOpRef ss' op) =
    for_ (getValueOpAlias op) \alias ->
      case alias of
        Left ident ->
          unless (Array.any (\r -> case r of
            ValueRef _ i -> i == ident
            _ -> false) exps)
            (throwError (errorMessage' ss' (TransitiveExportError dr [ValueRef ss' ident])))
        Right ctor ->
          unless (anyTypeRef (\(Tuple _ mctors) ->
            case mctors of
              Just ctors -> Array.any (_ == ctor) ctors
              Nothing    -> false))
            (throwError (errorMessage' ss (TransitiveDctorExportError dr [ctor])))
  checkRef dr@(TypeOpRef ss' op) =
    for_ (getTypeOpAlias op) \ty ->
      unless (anyTypeRef (\(Tuple n _) -> n == ty))
        (throwError (errorMessage' ss' (TransitiveExportError dr [TypeRef ss' ty Nothing])))
  checkRef _ = pure unit

  getTypeOpAlias :: OpName TypeOpName -> Maybe (ProperName TypeName)
  getTypeOpAlias op = Array.head (Array.mapMaybe go ds)
    where
    go d = do
      fixity <- getFixityDecl d
      case fixity of
        Right (TypeFixity _ (Qualified (ByModuleName mn') ident) op')
          | mn == mn' && op == op' -> Just ident
        _ -> Nothing

  getValueOpAlias :: OpName ValueOpName -> Maybe (Either Ident (ProperName ConstructorName))
  getValueOpAlias op = Array.head (Array.mapMaybe go ds)
    where
    go d = do
      fixity <- getFixityDecl d
      case fixity of
        Left (ValueFixity _ (Qualified (ByModuleName mn') ident) op')
          | mn == mn' && op == op' -> Just ident
        _ -> Nothing

  anyTypeRef :: (Tuple (ProperName TypeName) (Maybe (Array (ProperName ConstructorName))) -> Boolean) -> Boolean
  anyTypeRef f = Array.any (\ref -> case ref of
    TypeRef _ n mctors -> f (Tuple n mctors)
    _ -> false) exps

usingPredicate :: forall f a. Applicative f => (a -> Boolean) -> (a -> f a) -> a -> f a
usingPredicate p f x = if p x then f x else pure x

unless :: forall m. Applicative m => Boolean -> m Unit -> m Unit
unless true  _ = pure unit
unless false m = m
