module Utils.HaskellDataflow.Graph 
( module Utils.HaskellDataflow.Graph.Types
, module Utils.HaskellDataflow.Graph.BellmanFord
, module Utils.HaskellDataflow.Graph.DFS
--, dfsHO
--, dfsU
--, dfsGU
--, bellmanFord
, findPathInTreeEdgeList
) where

import Prelude

import Utils.HaskellDataflow.Graph.Types
import Utils.HaskellDataflow.Graph.BellmanFord
import Utils.HaskellDataflow.Graph.DFS


--- TODO move to a separate module?

import qualified Data.Map as M
import qualified Data.List as L
    
findPathInTreeEdgeList ::
    (Edges e, Ord l) =>
    [e l] ->
    l ->
    l ->
    Maybe [e l]
findPathInTreeEdgeList es from to
    | from == to    = Just []
    | null ys       = Nothing
    | otherwise     = Just $ head ys : reverse xs
    where
      treeMap       = M.fromList [(target edge, edge) | edge <- es] 
      fun lbl       = fmap unwrap $ M.lookup lbl treeMap
      unwrap edge   = (edge, source edge)
      ancestors     = L.unfoldr fun to
      (xs, ys)      = L.span (\edge -> source edge /= from) ancestors


testTree :: [Edge Char]
testTree = [Edge 'h' 'a', Edge 'a' 'b', Edge 'b' 'c', Edge 'b' 'd']

