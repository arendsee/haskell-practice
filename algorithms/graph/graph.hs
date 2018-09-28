module graph
(
  Graph(..)
) where

data Graph e v = Graph {
    edgelist :: [(Int, Int)]
    edgeattr :: [((Int, Int), e)] 
    vertattr :: [(Int, v)]
  }

getVertexAttr :: Graph e v -> Int -> Maybe v 
getVertexAttr = undefined
