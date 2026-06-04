-- | Laziness transform for CoreFn.
-- |
-- | This module ensures that recursive binding groups are initialized in a
-- | valid order by detecting dependency cycles and wrapping cyclic bindings
-- | in lazy thunks ($runtime_lazy) where necessary.
module Language.PureScript.CoreFn.Laziness
  ( applyLazinessTransform
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)

import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (SourcePos(..), SourceSpan, nullSourceSpan, spanStart)
import Language.PureScript.Constants.Libs as C
import Language.PureScript.CoreFn.Ann (Ann, ssAnn)
import Language.PureScript.CoreFn.Expr (Bind(..), CaseAlternative(..), Expr(..))
import Language.PureScript.Names
  ( Ident(..)
  , InternalIdentData(..)
  , ModuleName
  , Qualified(..)
  , QualifiedBy(..)
  , byNullSourcePos
  , runIdent
  , runModuleName
  , toMaybeModuleName
  )
import Language.PureScript.PSString (mkString)

-- ---------------------------------------------------------------------------
-- Top-level entry point
-- ---------------------------------------------------------------------------

-- | Transform a list of CoreFn bindings, introducing lazy initializers where
-- | needed to handle potential initialization order issues in recursive groups.
applyLazinessTransform :: ModuleName -> Array (Bind Ann) -> Array (Bind Ann)
applyLazinessTransform mn = Array.concatMap (transformBind mn)

-- ---------------------------------------------------------------------------
-- Bind transformation
-- ---------------------------------------------------------------------------

transformBind :: ModuleName -> Bind Ann -> Array (Bind Ann)
transformBind _  b@(NonRec _ _ _) = [ b ]
transformBind mn (Rec items)       = transformRecGroup mn items

-- ---------------------------------------------------------------------------
-- Recursive group transformation
-- ---------------------------------------------------------------------------

-- | Transform a single recursive binding group.
-- |
-- | Algorithm:
-- | 1. Collect all names in this binding group.
-- | 2. For each binding, find which other group names it directly references
-- |    at delay-0 (i.e., not guarded by a lambda).
-- | 3. Find cycles using DFS / reachability.
-- | 4. Cyclic bindings get split into:
-- |    - A lazy definition: $lazy_x = runFn3 runtimeLazy "x" "Mod" (\_ -> body)
-- |    - A lazy binding:    x = $lazy_x lineNumber
-- |    References to cyclic bindings within all expressions are rewritten as
-- |    force calls: $lazy_x lineNumber
-- | 5. Non-cyclic bindings are emitted as NonRec.
transformRecGroup
  :: ModuleName
  -> Array (Tuple (Tuple Ann Ident) (Expr Ann))
  -> Array (Bind Ann)
transformRecGroup mn items =
  let
    -- Build a set of names in this group
    names :: Set Ident
    names = Set.fromFoldable (map (snd <<< fst) items)

    -- For each binding, find all group-member names it references at delay-0
    deps :: Map Ident (Set Ident)
    deps = Map.fromFoldable $ map
      (\(Tuple (Tuple _ ident) expr) ->
        Tuple ident (directRefs mn names expr))
      items

    -- Find which names are part of a cycle
    cyclicNames :: Set Ident
    cyclicNames = findCyclicNames deps names

  in if Set.isEmpty cyclicNames
     then
       -- No cycles: emit as a single Rec (keeps original structure)
       [ Rec items ]
     else
       -- Split cyclic bindings into lazy defs + force bindings,
       -- rewrite references to cyclic names as force calls
       let rewriteExpr = replaceRefs mn cyclicNames
       in Array.concatMap (emitBinding mn rewriteExpr cyclicNames) items

-- | Emit one binding from a recursive group.
emitBinding
  :: ModuleName
  -> (Expr Ann -> Expr Ann)
  -> Set Ident
  -> Tuple (Tuple Ann Ident) (Expr Ann)
  -> Array (Bind Ann)
emitBinding mn rewriteExpr cyclicNames (Tuple (Tuple ann ident) expr) =
  if Set.member ident cyclicNames
  then
    let
      lazyIdent = lazifyIdent ident
      lazyBody  = rewriteExpr expr
      lazyExpr  = makeLazyDef mn ident lazyBody
      forceExpr = makeForceCall ann ident
    in
      [ NonRec nullAnn lazyIdent lazyExpr
      , NonRec ann ident forceExpr
      ]
  else
    -- Non-cyclic: emit as NonRec with references rewritten
    [ NonRec ann ident (rewriteExpr expr) ]

-- ---------------------------------------------------------------------------
-- Constructing lazy definitions and force calls
-- ---------------------------------------------------------------------------

nullAnn :: Ann
nullAnn = ssAnn nullSourceSpan

-- | $runtime_lazy (the factory function)
runtimeLazy :: Expr Ann
runtimeLazy = Var nullAnn (Qualified byNullSourcePos (InternalIdent RuntimeLazyFactory))

-- | runFn3 from Data.Function.Uncurried
runFn3 :: Expr Ann
runFn3 = Var nullAnn
  (Qualified (ByModuleName C.mDataFunctionUncurried)
    (Ident (C.sRunFn <> "3")))

-- | Build a string literal expression
strLit :: String -> Expr Ann
strLit s = Literal nullAnn (StringLiteral (mkString s))

-- | Build a lazy definition:
-- |   runFn3 $runtime_lazy identStr moduleStr (\_ -> body)
makeLazyDef :: ModuleName -> Ident -> Expr Ann -> Expr Ann
makeLazyDef mn ident body =
  App nullAnn
    (App nullAnn
      (App nullAnn
        (App nullAnn runFn3 runtimeLazy)
        (strLit (runIdent ident)))
      (strLit (runModuleName mn)))
    (Abs nullAnn UnusedIdent body)

-- | Build a force call: $lazy_x lineNumber
makeForceCall :: Ann -> Ident -> Expr Ann
makeForceCall (Tuple ss _) ident =
  App nullAnn
    (Var nullAnn (Qualified byNullSourcePos (lazifyIdent ident)))
    (Literal nullAnn (NumericLiteral (Left (sourcePosLine (spanStart ss)))))

-- | Convert an ident to its lazy version: Ident "foo" -> InternalIdent (Lazy "foo")
lazifyIdent :: Ident -> Ident
lazifyIdent (Ident txt) = InternalIdent (Lazy txt)
lazifyIdent other = other

sourcePosLine :: SourcePos -> Int
sourcePosLine (SourcePos { line }) = line

-- ---------------------------------------------------------------------------
-- Finding direct (delay-0) references
-- ---------------------------------------------------------------------------

-- | Collect all names from the given set that appear at delay-0 in the expression.
-- | Delay-0 means: not guarded by a lambda (Abs).
-- |
-- | This is a simplified version of the Haskell delay/force analysis:
-- | - References inside Abs bodies have delay > 0 and are excluded
-- | - All other positions are treated as delay 0 (conservative)
directRefs :: ModuleName -> Set Ident -> Expr Ann -> Set Ident
directRefs mn names = go
  where
  go :: Expr Ann -> Set Ident
  go (Var _ (Qualified qb ident))
    | qualifiedByModule mn qb
    , Set.member ident names
    = Set.singleton ident
  go (Var _ _)                = Set.empty
  go (Literal _ _)            = Set.empty
  go (Constructor _ _ _ _)    = Set.empty
  go (Accessor _ _ e)         = go e
  go (ObjectUpdate _ e _ vs)  =
    Set.union (go e) (foldl (\acc (Tuple _ v) -> Set.union acc (go v)) Set.empty vs)
  go (Abs _ _ _)              = Set.empty   -- delay > 0: excluded
  go (App _ f a)              = Set.union (go f) (go a)
  go (Case _ vs alts)         =
    let vsRefs  = foldl (\acc v -> Set.union acc (go v)) Set.empty vs
        altRefs = foldl (\acc alt -> Set.union acc (goAlt alt)) Set.empty alts
    in Set.union vsRefs altRefs
  go (Let _ binds body)       =
    let bindRefs = foldl (\acc b -> Set.union acc (goBind b)) Set.empty binds
    in Set.union bindRefs (go body)

  goBind :: Bind Ann -> Set Ident
  goBind (NonRec _ _ e) = go e
  goBind (Rec items)    =
    foldl (\acc (Tuple _ e) -> Set.union acc (go e)) Set.empty items

  goAlt :: CaseAlternative Ann -> Set Ident
  goAlt (CaseAlternative ca) = case ca.caseAlternativeResult of
    Left guards ->
      foldl (\acc (Tuple g e) -> Set.union acc (Set.union (go g) (go e))) Set.empty guards
    Right e -> go e

-- | Is this QualifiedBy consistent with the given module (or unqualified)?
qualifiedByModule :: ModuleName -> QualifiedBy -> Boolean
qualifiedByModule mn (ByModuleName mn') = mn == mn'
qualifiedByModule _  (BySourcePos _)   = true

-- ---------------------------------------------------------------------------
-- Cycle detection
-- ---------------------------------------------------------------------------

-- | Find all names that participate in at least one cycle in the dependency graph.
findCyclicNames :: Map Ident (Set Ident) -> Set Ident -> Set Ident
findCyclicNames deps names =
  let nameArr = Array.fromFoldable names
      isCyclic name =
        let directDeps = fromMaybe Set.empty (Map.lookup name deps)
            reached    = bfsReachable deps directDeps directDeps
        in Set.member name reached
  in Set.fromFoldable (Array.filter isCyclic nameArr)

-- | BFS reachability from a set of nodes, staying within the given dep map.
bfsReachable :: Map Ident (Set Ident) -> Set Ident -> Set Ident -> Set Ident
bfsReachable deps visited frontier =
  if Set.isEmpty frontier
  then visited
  else
    let nextNodes = foldl
          (\acc ident ->
            Set.union acc (fromMaybe Set.empty (Map.lookup ident deps)))
          Set.empty
          (Array.fromFoldable frontier)
        newNodes = Set.difference nextNodes visited
    in bfsReachable deps (Set.union visited newNodes) newNodes

-- ---------------------------------------------------------------------------
-- Reference replacement
-- ---------------------------------------------------------------------------

-- | Replace all direct references to cyclic names in an expression with force calls.
replaceRefs :: ModuleName -> Set Ident -> Expr Ann -> Expr Ann
replaceRefs mn cyclicNames = goExpr
  where
  goExpr :: Expr Ann -> Expr Ann
  goExpr v@(Var ann (Qualified qb ident))
    | qualifiedByModule mn qb
    , Set.member ident cyclicNames
    = makeForceCall ann ident
  goExpr (Var ann q)            = Var ann q
  goExpr (Literal ann lit)      = Literal ann lit
  goExpr (Constructor ann t c fs) = Constructor ann t c fs
  goExpr (Accessor ann prop e)  = Accessor ann prop (goExpr e)
  goExpr (ObjectUpdate ann obj copy vs) =
    ObjectUpdate ann (goExpr obj) copy (map (\(Tuple k v) -> Tuple k (goExpr v)) vs)
  goExpr (Abs ann i e)          = Abs ann i (goExpr e)
  goExpr (App ann f a)          = App ann (goExpr f) (goExpr a)
  goExpr (Case ann vs alts)     = Case ann (map goExpr vs) (map goAlt alts)
  goExpr (Let ann binds body)   = Let ann (map goBind binds) (goExpr body)

  goBind :: Bind Ann -> Bind Ann
  goBind (NonRec ann i e) = NonRec ann i (goExpr e)
  goBind (Rec items)      = Rec (map (\(Tuple ai e) -> Tuple ai (goExpr e)) items)

  goAlt :: CaseAlternative Ann -> CaseAlternative Ann
  goAlt (CaseAlternative ca) = CaseAlternative ca
    { caseAlternativeResult = case ca.caseAlternativeResult of
        Left guards ->
          Left (map (\(Tuple g e) -> Tuple (goExpr g) (goExpr e)) guards)
        Right e -> Right (goExpr e)
    }
