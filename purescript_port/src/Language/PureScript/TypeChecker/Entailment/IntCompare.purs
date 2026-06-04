-- | Stub for the type-level Int comparison solver.
-- | The full graph-based solver from the Haskell source uses Data.Graph;
-- | this stub implements basic literal comparison and defers the
-- | constraint-propagation cases.
module Language.PureScript.TypeChecker.Entailment.IntCompare
  ( Relation(..)
  , Context
  , mkFacts
  , mkRelation
  , solveRelation
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))

import Language.PureScript.Constants.Prim as C
import Language.PureScript.Names (Qualified, TypeName, ProperName)
import Language.PureScript.Types (SourceType, Type(..))

-- | A simple ordering relation between two type-level ints
data Relation a
  = Equal a a
  | LessThan a a

type Context a = Array (Relation a)

-- | Build a relation from a dict's instance types [a, b, ordering]
mkRelation :: SourceType -> SourceType -> SourceType -> Maybe (Relation SourceType)
mkRelation a b (TypeConstructor _ ord)
  | ord == C.tyEQ = Just (Equal a b)
  | ord == C.tyLT = Just (LessThan a b)
  | ord == C.tyGT = Just (LessThan b a)
mkRelation _ _ _ = Nothing

-- | Build facts from all the available IntCompare instance types
mkFacts :: Array (Array SourceType) -> Context SourceType
mkFacts argArrays = Array.catMaybes do
  args <- argArrays
  case args of
    [a, b, ord] -> [mkRelation a b ord]
    _           -> []

-- | Attempt to solve the ordering between two type-level ints using the context
solveRelation
  :: Context SourceType
  -> SourceType
  -> SourceType
  -> Maybe (Qualified (ProperName TypeName))
solveRelation _ctx (TypeLevelInt _ a) (TypeLevelInt _ b) =
  case compare a b of
    EQ -> Just C.tyEQ
    LT -> Just C.tyLT
    GT -> Just C.tyGT
solveRelation _ctx _ _ = Nothing
