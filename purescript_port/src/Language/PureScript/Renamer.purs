-- | Renaming pass that prevents shadowing of local identifiers.
module Language.PureScript.Renamer (renameInModule) where

import Prelude

import Control.Monad.State (State, gets, modify_, runState)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)

import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.CoreFn.Binders (Binder(..))
import Language.PureScript.CoreFn.Expr
  ( Bind(..)
  , CaseAlternative(..)
  , Expr(..)
  )
import Language.PureScript.CoreFn.Module (Module(..))
import Language.PureScript.Names (Ident(..), Qualified(..), isBySourcePos, isPlainIdent, runIdent, showIdent)

type RenameState =
  { rsBoundNames :: Map Ident Ident
  , rsUsedNames  :: Set Ident
  }

type Rename a = State RenameState a

initState :: Array Ident -> RenameState
initState scope =
  { rsBoundNames: Map.fromFoldable (map (\i -> Tuple i i) scope)
  , rsUsedNames:  Set.fromFoldable scope
  }

runRename :: forall a. Array Ident -> Rename a -> Tuple a RenameState
runRename scope action = runState action (initState scope)

newScope :: forall a. Rename a -> Rename a
newScope action = do
  scope <- gets identity
  a <- action
  modify_ (const scope)
  pure a

updateScope :: Ident -> Rename Ident
updateScope ident =
  case ident of
    GenIdent name _ -> go ident (Ident (fromMaybe "v" name))
    UnusedIdent -> pure UnusedIdent
    _ -> go ident ident
  where
  go :: Ident -> Ident -> Rename Ident
  go keyName baseName = do
    usedNames <- gets _.rsUsedNames
    let name' =
          if Set.member baseName usedNames
          then getNewName usedNames baseName
          else baseName
    modify_ \s -> s
      { rsBoundNames = Map.insert keyName name' s.rsBoundNames
      , rsUsedNames  = Set.insert name' s.rsUsedNames
      }
    pure name'

  getNewName :: Set Ident -> Ident -> Ident
  getNewName usedNames name =
    unsafePartial $ fromJust $ Array.findMap
      (\i ->
        let candidate = Ident (runIdent name <> show (i :: Int))
        in if not (Set.member candidate usedNames) then Just candidate else Nothing)
      (Array.range 1 10000)

lookupIdent :: Ident -> Rename Ident
lookupIdent UnusedIdent = pure UnusedIdent
lookupIdent name = do
  name' <- gets (Map.lookup name <<< _.rsBoundNames)
  case name' of
    Just n  -> pure n
    Nothing -> pure name

-- | Renames within each declaration in a module.
renameInModule :: forall a. Module a -> Tuple (Map Ident Ident) (Module a)
renameInModule (Module m) =
  let Tuple (Tuple moduleDecls moduleExports) rs =
        runRename m.moduleForeign
          (Tuple <$> renameInDecls m.moduleDecls <*> traverse lookupIdent m.moduleExports)
  in Tuple rs.rsBoundNames (Module m { moduleDecls = moduleDecls, moduleExports = moduleExports })

renameInDecls :: forall a. Array (Bind a) -> Rename (Array (Bind a))
renameInDecls decls = do
  decls1 <- traverse (renameDecl false) decls
  decls2 <- traverse (renameDecl true) decls1
  traverse renameValuesInDecl decls2
  where
  renameDecl :: Boolean -> Bind a -> Rename (Bind a)
  renameDecl isSecondPass = case _ of
    NonRec a name val -> do
      name' <- updateName name
      pure (NonRec a name' val)
    Rec ds -> Rec <$> traverse updateNames ds
    where
    updateName :: Ident -> Rename Ident
    updateName name =
      (if isSecondPass == isPlainIdent name then pure else updateScope) name

    updateNames :: Tuple (Tuple a Ident) (Expr a) -> Rename (Tuple (Tuple a Ident) (Expr a))
    updateNames (Tuple (Tuple ann name) val) = do
      name' <- updateName name
      pure (Tuple (Tuple ann name') val)

  renameValuesInDecl :: Bind a -> Rename (Bind a)
  renameValuesInDecl = case _ of
    NonRec a name val -> NonRec a name <$> renameInValue val
    Rec ds -> Rec <$> traverse (\(Tuple aname val) -> Tuple aname <$> renameInValue val) ds

renameInValue :: forall a. Expr a -> Rename (Expr a)
renameInValue (Literal ann l) =
  Literal ann <$> renameInLiteral renameInValue l
renameInValue c@(Constructor _ _ _ _) = pure c
renameInValue (Accessor ann prop v) =
  Accessor ann prop <$> renameInValue v
renameInValue (ObjectUpdate ann obj copy vs) =
  ObjectUpdate ann
    <$> renameInValue obj
    <*> pure copy
    <*> traverse (\(Tuple name v) -> Tuple name <$> renameInValue v) vs
renameInValue (Abs ann name v) =
  newScope $ Abs ann <$> updateScope name <*> renameInValue v
renameInValue (App ann v1 v2) =
  App ann <$> renameInValue v1 <*> renameInValue v2
renameInValue (Var ann (Qualified qb name))
  | isBySourcePos qb || not (isPlainIdent name) =
    Var ann <<< Qualified qb <$> lookupIdent name
renameInValue v@(Var _ _) = pure v
renameInValue (Case ann vs alts) =
  newScope $ Case ann <$> traverse renameInValue vs <*> traverse renameInCaseAlternative alts
renameInValue (Let ann ds v) =
  newScope $ Let ann <$> renameInDecls ds <*> renameInValue v

renameInLiteral :: forall a. (a -> Rename a) -> Literal a -> Rename (Literal a)
renameInLiteral rename (ArrayLiteral bs)  = ArrayLiteral <$> traverse rename bs
renameInLiteral rename (ObjectLiteral bs) = ObjectLiteral <$> traverse (\(Tuple k v) -> Tuple k <$> rename v) bs
renameInLiteral _ l = pure l

renameInCaseAlternative :: forall a. CaseAlternative a -> Rename (CaseAlternative a)
renameInCaseAlternative (CaseAlternative c) = newScope do
  binders <- traverse renameInBinder c.caseAlternativeBinders
  result  <- case c.caseAlternativeResult of
    Left guards -> Left <$> traverse (\(Tuple g e) -> Tuple <$> renameInValue g <*> renameInValue e) guards
    Right e     -> Right <$> renameInValue e
  pure (CaseAlternative { caseAlternativeBinders: binders, caseAlternativeResult: result })

renameInBinder :: forall a. Binder a -> Rename (Binder a)
renameInBinder n@(NullBinder _) = pure n
renameInBinder (LiteralBinder ann b) =
  LiteralBinder ann <$> renameInLiteral renameInBinder b
renameInBinder (VarBinder ann name) =
  VarBinder ann <$> updateScope name
renameInBinder (ConstructorBinder ann tctor dctor bs) =
  ConstructorBinder ann tctor dctor <$> traverse renameInBinder bs
renameInBinder (NamedBinder ann name b) =
  NamedBinder ann <$> updateScope name <*> renameInBinder b
