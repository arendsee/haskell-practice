{- READ: Structuring Depth-First Search Algorithms in Haskell, by David King and John Launchbury. -}

import Data.Graph
import Control.Monad

nmap :: (a -> b) -> (a, k, ks) -> (b, k, ks)
nmap f (x, k, ks) = (f x, k, ks)

main :: IO ()
main = do
  -- define an edge graph, vertices are defined as (node, key, [key])
  let edges = [(45, 1, [2,3]), (89, 2, []), (1113, 3, [])]
  -- graphFromEdges :: Ord key =>
  --    [(node, key, [key])] ->
  --    (
  --         Graph
  --       , Vertex -> (node, key, [key])
  --       , key -> Maybe Vertex
  --    )
  let (graph, fv, fk) = graphFromEdges edges
  mapM_ (print . liftM fv . fk) [1..4]
  print " ---- "
  mapM_ (print . fv) (vertices graph)
  print " ---- "
  let edges2 = map (nmap (* 2)) $ map fv $ vertices graph
  let (graph2, fv2, fk2) = graphFromEdges edges2
  mapM_ (print . fv) (vertices graph2)
