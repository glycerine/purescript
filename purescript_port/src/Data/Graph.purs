-- | Port of Haskell's Data.Graph SCC functionality using Tarjan's algorithm.
module Data.Graph
  ( SCC(..)
  , stronglyConnComp
  , stronglyConnCompR
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))

data SCC a
  = AcyclicSCC a
  | CyclicSCC (Array a)

-- | Compute strongly-connected components.
-- Input: array of Tuple node (Tuple key (Array depKey))
-- Returns SCCs in reverse topological order.
stronglyConnComp
  :: forall node key
   . Ord key
  => Array (Tuple node (Tuple key (Array key)))
  -> Array (SCC node)
stronglyConnComp verts =
  map (mapSCC (\(Tuple n _) -> n))
    (stronglyConnCompR
      (map (\(Tuple nd (Tuple k ds)) -> Tuple (Tuple nd k) (Tuple k ds)) verts))
  where
  mapSCC f (AcyclicSCC x) = AcyclicSCC (f x)
  mapSCC f (CyclicSCC xs) = CyclicSCC (map f xs)

-- | Like stronglyConnComp but returns the full vertex Tuple node (Tuple key (Array depKey)) in each SCC.
stronglyConnCompR
  :: forall node key
   . Ord key
  => Array (Tuple (Tuple node key) (Tuple key (Array key)))
  -> Array (SCC (Tuple node (Tuple key (Array key))))
stronglyConnCompR verts = runTarjan
  where
  n = Array.length verts

  keyToIdx = Map.fromFoldable
    (Array.mapWithIndex (\i (Tuple (Tuple _ k) _) -> Tuple k i) verts)

  nodeAt i = case Array.index verts i of
    Just (Tuple (Tuple nd k) (Tuple _ ds)) -> Tuple nd (Tuple k ds)
    Nothing -> unsafeCrash "nodeAt: out of bounds"

  depsOf i = case Array.index verts i of
    Just (Tuple _ (Tuple _ ds)) ->
      Array.mapMaybe (\k -> Map.lookup k keyToIdx) ds
    Nothing -> []

  initState =
    { counter:  0
    , indices:  (Map.empty :: Map Int Int)
    , lowlinks: (Map.empty :: Map Int Int)
    , onStack:  (Map.empty :: Map Int Boolean)
    , stack:    ([] :: Array Int)
    , result:   []
    }

  runTarjan =
    if n == 0 then []
    else
      let finalState = Array.foldl visit initState (Array.range 0 (n - 1))
      in finalState.result

  visit st v
    | Map.member v st.indices = st
    | otherwise = strongconnect st v

  strongconnect st0 v =
    let st1 = st0
              { counter  = st0.counter + 1
              , indices  = Map.insert v st0.counter st0.indices
              , lowlinks = Map.insert v st0.counter st0.lowlinks
              , stack    = Array.cons v st0.stack
              , onStack  = Map.insert v true st0.onStack
              }
        st2 = Array.foldl (processEdge v) st1 (depsOf v)
        vLow = fromMaybe 0 (Map.lookup v st2.lowlinks)
        vIdx = fromMaybe 0 (Map.lookup v st2.indices)
    in if vLow == vIdx then popSCC st2 v else st2

  processEdge v st w
    | not (Map.member w st.indices) =
        let st' = strongconnect st w
            wLow = fromMaybe 0 (Map.lookup w st'.lowlinks)
            vLow = fromMaybe 0 (Map.lookup v st'.lowlinks)
        in st' { lowlinks = Map.insert v (min vLow wLow) st'.lowlinks }
    | fromMaybe false (Map.lookup w st.onStack) =
        let wIdx = fromMaybe 0 (Map.lookup w st.indices)
            vLow = fromMaybe 0 (Map.lookup v st.lowlinks)
        in st { lowlinks = Map.insert v (min vLow wIdx) st.lowlinks }
    | otherwise = st

  popSCC st root =
    let Tuple members rest = splitAtRoot st.stack
        st2 = st
          { stack   = rest
          , onStack = Array.foldl (\m i -> Map.insert i false m) st.onStack members
          }
        sccNodes = map nodeAt members
        scc = case sccNodes of
          [single] ->
            if Array.any (_ == root) (depsOf root)
              then CyclicSCC sccNodes
              else AcyclicSCC single
          _ -> CyclicSCC sccNodes
    in st2 { result = Array.snoc st2.result scc }
    where
    splitAtRoot arr = go [] arr
      where
      go acc remaining = case Array.uncons remaining of
        Nothing -> Tuple (Array.reverse acc) []
        Just { head: x, tail: xs } ->
          if x == root
            then Tuple (Array.reverse (Array.cons x acc)) xs
            else go (Array.cons x acc) xs

foreign import unsafeCrash :: forall a. String -> a
