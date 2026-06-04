-- | Stub for the Coercible constraint solver.
-- | The full interaction-based solver from the Haskell source is complex;
-- | this stub provides the minimal interface needed by Entailment.purs.
module Language.PureScript.TypeChecker.Entailment.Coercible
  ( GivenSolverState(..)
  , WantedSolverState(..)
  , initialGivenSolverState
  , initialWantedSolverState
  , insoluble
  , solveGivens
  , solveWanteds
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.State (StateT)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Writer.Trans (WriterT)
import Data.Tuple (Tuple(..))

import Language.PureScript.Environment (Environment)
import Language.PureScript.Errors (MultipleErrors, SimpleErrorMessage(..), errorMessage)
import Language.PureScript.Types (SourceType)

-- | State for the "given" (known) Coercible constraints solver
newtype GivenSolverState = GivenSolverState
  { inertGivens :: Array (Tuple SourceType SourceType)
  }

-- | State for the "wanted" (goal) Coercible constraints solver
newtype WantedSolverState = WantedSolverState
  -- | inertWanteds: list of (kind, lhs, rhs) tuples that couldn't be solved
  { inertWanteds :: Array (Tuple SourceType (Tuple SourceType SourceType))
  }

initialGivenSolverState
  :: Array (Tuple SourceType SourceType)
  -> GivenSolverState
initialGivenSolverState givens = GivenSolverState { inertGivens: givens }

initialWantedSolverState
  :: Array (Tuple SourceType SourceType)
  -> SourceType
  -> SourceType
  -> WantedSolverState
initialWantedSolverState _inertGivens _a _b =
  WantedSolverState { inertWanteds: [] }
  -- Stub: we optimistically say a ~ b is solvable (empty inertWanteds = success)
  -- A real implementation would do interaction-based solving here.

solveGivens
  :: forall m
   . MonadState GivenSolverState m
  => MonadError MultipleErrors m
  => Environment
  -> m Unit
solveGivens _env = pure unit

-- | solveWanteds runs in StateT WantedSolverState (WriterT MultipleErrors m)
-- | matching the Haskell: StateT WantedSolverState (WriterT [ErrorMessageHint] m)
solveWanteds
  :: forall m
   . Monad m
  => Environment
  -> StateT WantedSolverState (WriterT MultipleErrors m) Unit
solveWanteds _env = pure unit

-- | Build the "insoluble Coercible" error message
insoluble :: SourceType -> SourceType -> SourceType -> MultipleErrors
insoluble _k _a _b =
  errorMessage PossiblyInfiniteCoercibleInstance
