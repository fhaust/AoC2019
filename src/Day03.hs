{-# LANGUAGE ViewPatterns #-}

import           Data.List
import           Data.List.Split

import           Data.Function
import           Data.Maybe

import qualified Data.Set as Set


-- TODO ... clean up this mess

parseInput = map go . splitOn "," . init
  where go (x:xs) = (x, read xs :: Int)

dirToVec 'R' = (1,0)
dirToVec 'L' = (-1,0)
dirToVec 'U' = (0,1)
dirToVec 'D' = (0,-1)

svMul a (x,y) = (a*x, a*y)
vvAdd (ax,ay) (bx,by) = (ax+bx, ay+by)

unrollWire cp dirs = go cp $ dirs
  where
    go cp ((dirToVec -> dir, dist) : dirs) | null dirs = ps
                                           | otherwise = ps ++ go (last ps) dirs
      where ps = [ cp `vvAdd` (i `svMul` dir) | i <- [1..dist] ]

mhDist (x,y) = abs x + abs y

part1 w0 w1 = mhDist $ minimumBy (compare `on` mhDist) $ intersections
  where
    intersections = Set.intersection u0 u1
    u0            = Set.fromList $ unrollWire (0,0) w0
    u1            = Set.fromList $ unrollWire (0,0) w1


-- part 2
part2 w0 w1 =  (+2) . fromJust . minimum $ Set.toList $  Set.map (\i -> (+) <$> findIndex (\x -> x == i) u0 <*> findIndex (\x -> x == i) u1) intersections
  where
    intersections = Set.intersection (Set.fromList u0) (Set.fromList u1)
    u0            = unrollWire (0,0) w0
    u1            = unrollWire (0,0) w1

main = do

  [w0, w1] <- map parseInput . lines <$> readFile "inputs/day03.txt"

  putStrLn $ "Result 1: " ++ show (part1 w0 w1)

  putStrLn $ "Result 2: " ++ show (part2 w0 w1)

  putStrLn "done"
