
import           Data.List
import           Data.List.Split

import           Data.Tree
import           Data.Graph
import qualified Data.Map.Strict as M

parseInput = map (splitOn ")") . lines

testInput1 = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"

testInput2 = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"

-- | create graph from input
-- bi controls if reverse connections (needed for part 2) are inserted
starMap bi = graphFromEdges
           . map (\(a,b) -> (a,a,b))
           . M.toList
           . foldl (\m [k,v] -> M.insertWith (++) v (if bi then [k] else [])
                              . M.insertWith (++) k [v]
                              $ m
                   ) M.empty 
           . parseInput

-- | return a list of all orbits that affect an object
orbits (graph,_,_) x = tail $ concatMap flatten $ dfs (transposeG graph) [x]

-- | get the number of orbits for all objects and sum them up
numberOfOrbits sm@(graph, _, _) = sum [ length $ orbits sm v | v <- vertices graph ]


-- | find the distance from one node to another ... poor mans path finding ;-)
shortestPath (graph, nodeFromVertex, vertexFromKey) a b = subtract 2 <$> san2you
  where
    (Just san) = vertexFromKey a
    (Just you) = vertexFromKey b
    san2you    = findIndex (san `elem`) . levels . head . dfs graph $ [you]

-- | main program
main = do

  f <- readFile "inputs/day06.txt"

  let result1        = numberOfOrbits (starMap False f)
      (Just result2) = shortestPath (starMap True f) "YOU" "SAN"

  putStrLn $ "Result1: " ++ show result1
  putStrLn $ "Result2: " ++ show result2


