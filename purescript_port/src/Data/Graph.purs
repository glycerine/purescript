-- | Port of Haskell's Data.Graph SCC functionality.
-- | Implements containers-0.6.8's algorithm:
-- |   scc g = dfs g (reverse (postOrd (transposeG g)))
-- | where postOrd uses DFS on the TRANSPOSED graph,
-- | and the second DFS on the ORIGINAL graph uses reversed postorder as
-- | starting vertices. This exactly matches Haskell's stronglyConnCompR output.
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
import Data.Ord (comparing)
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

-- | Like stronglyConnComp but returns the full vertex in each SCC.
-- | Implements containers-0.6.8's:
-- |   scc g = dfs g (reverse (postOrd (transposeG g)))
stronglyConnCompR
  :: forall node key
   . Ord key
  => Array (Tuple (Tuple node key) (Tuple key (Array key)))
  -> Array (SCC (Tuple node (Tuple key (Array key))))
stronglyConnCompR verts =
  if n == 0 then []
  else map decode sccTrees
  where
  -- Sort vertices by key ascending (matching graphFromEdges)
  sorted = Array.sortBy (comparing (\(Tuple (Tuple _ k) _) -> k)) verts
  n = Array.length sorted

  keyToIdx :: Map key Int
  keyToIdx = Map.fromFoldable
    (Array.mapWithIndex (\i (Tuple (Tuple _ k) _) -> Tuple k i) sorted)

  nodeAt :: Int -> Tuple node (Tuple key (Array key))
  nodeAt i = case Array.index sorted i of
    Just (Tuple (Tuple nd k) (Tuple _ ds)) -> Tuple nd (Tuple k ds)
    Nothing -> unsafeCrash "nodeAt: out of bounds"

  -- Adjacency lists: adj[i] = outgoing neighbors of i (i depends on these)
  adjList :: Array (Array Int)
  adjList = map (\(Tuple _ (Tuple _ ds)) -> Array.mapMaybe (\k -> Map.lookup k keyToIdx) ds) sorted

  adj :: Int -> Array Int
  adj i = fromMaybe [] (Array.index adjList i)

  -- Transposed adjacency lists: adjT[i] = vertices that depend on i
  adjTList :: Array (Array Int)
  adjTList = Array.foldl
    (\acc (Tuple i neighbors) ->
      Array.foldl
        (\a w ->
          case Array.index a w of
            Nothing -> a
            Just ws -> fromMaybe a (Array.updateAt w (Array.snoc ws i) a))
        acc
        neighbors)
    (Array.replicate n [])
    (Array.mapWithIndex Tuple adjList)

  adjT :: Int -> Array Int
  adjT i = fromMaybe [] (Array.index adjTList i)

  -- Step 2: DFS on transposed graph from [0..n-1], compute postorder
  -- postorder: children before parent
  postOrdT :: Array Int
  postOrdT = _.postorder $
    Array.foldl visitT { visited: Map.empty, postorder: [] } (Array.range 0 (n - 1))
    where
    visitT st v
      | Map.member v st.visited = st
      | otherwise =
          let st1 = st { visited = Map.insert v unit st.visited }
              st2 = Array.foldl visitT st1 (adjT v)
          in st2 { postorder = Array.snoc st2.postorder v }

  -- Step 3: DFS on original graph from reversed postorder
  -- Each tree collects nodes in preorder (root first, then descendants)
  sccTrees :: Array { root :: Int, members :: Array Int }
  sccTrees = _.result $
    Array.foldl visitG { visited: Map.empty, result: [] } (Array.reverse postOrdT)
    where
    visitG st v
      | Map.member v st.visited = st
      | otherwise =
          let Tuple members newVisited = collectPreorder st.visited v
          in { visited: newVisited, result: Array.snoc st.result { root: v, members } }

    collectPreorder :: Map Int Unit -> Int -> Tuple (Array Int) (Map Int Unit)
    collectPreorder visited0 v =
      let visited1 = Map.insert v unit visited0
          Tuple childNodes visited2 = Array.foldl
            (\(Tuple acc vis) w ->
              if Map.member w vis then Tuple acc vis
              else
                let Tuple sub vis' = collectPreorder vis w
                in Tuple (acc <> sub) vis')
            (Tuple [] visited1)
            (adj v)
      in Tuple ([v] <> childNodes) visited2

  -- Step 4: decode each SCC tree
  decode :: { root :: Int, members :: Array Int } -> SCC (Tuple node (Tuple key (Array key)))
  decode { root: v, members } = case members of
    [_] ->
      if Array.any (_ == v) (adj v)
        then CyclicSCC [nodeAt v]
        else AcyclicSCC (nodeAt v)
    _ -> CyclicSCC (map nodeAt members)

foreign import unsafeCrash :: forall a. String -> a
