{-# LANGUAGE TupleSections #-}

import           Data.List
import           Data.Function

import qualified Data.Set as Set


-- input and parsing

parseInput :: String -> [(Double, Double)]
parseInput ls = concat
              $ zipWith
                  (\y xs -> map (\x -> (fromIntegral x, y)) xs)
                  [0..]
                  (map (findIndices (=='#')) . lines $ ls)

input = parseInput <$> readFile "inputs/day10.txt"


-- linear algebra

vsub (a,b) (c,d) = (a-c, b-d)
vlen (a,b)       = sqrt(a**2 + b**2)

vAngle (x,y) = let a = atan2 x (-y) in if a < 0 then a + 2 * pi else a
vAngleFrom a b = vAngle (b `vsub` a)

distance a b = vlen (a `vsub` b)



-- | assemble all asteroids that are visible from x
visibleFrom x xs = map (sortBy (compare `on` distance x)) 
                 . groupBy ((==) `on` vAngleFrom x) 
                 . sortBy (compare `on` vAngleFrom x) 
                 . filter (/= x) 
                 $ xs

-- | get best position for the new star base
bestByVisible xs = maximumBy (compare `on` (\x -> length . visibleFrom x . filter (/= x) $ xs)) xs

-- | calculate shooting order
shootingOrder x xs = concat . transpose . visibleFrom x $ xs



main = do

  asteroids <- input

  -- part 1
  let base       = bestByVisible asteroids
      numVisible = length $ visibleFrom base asteroids

  putStrLn $ "Result 1: " ++ show base ++ " " ++ show numVisible ++ " visible"

  -- part 2
  let order = shootingOrder base asteroids

  putStrLn $ "Result 2: " ++ show (order !! 199)








-- TESTS

-- map1 = parseInput $ unlines [".#..#"
--                             ,"....."
--                             ,"#####"
--                             ,"....#"
--                             ,"...##"
--                             ]
-- test1 = numBestByVisible map1 == 8

-- map2 = parseInput $ unlines ["......#.#."
--                             ,"#..#.#...."
--                             ,"..#######."
--                             ,".#.#.###.."
--                             ,".#..#....."
--                             ,"..#....#.#"
--                             ,"#..#....#."
--                             ,".##.#..###"
--                             ,"##...#..#."
--                             ,".#....####"
--                             ]
-- test2 = numBestByVisible map2 == 33

-- map3 = parseInput $ unlines
--         [".#..#..###"
--         ,"####.###.#"
--         ,"....###.#."
--         ,"..###.##.#"
--         ,"##.##.#.#."
--         ,"....###..#"
--         ,"..#.#..#.#"
--         ,"#..#.#.###"
--         ,".##...##.#"
--         ,".....#.#.."
--         ]
-- test3 = numBestByVisible map3 == 41

-- map4base = (11,13) :: (Double, Double)
-- map4 = parseInput $ unlines
--         [".#..##.###...#######"
--         ,"##.############..##."
--         ,".#.######.########.#"
--         ,".###.#######.####.#."
--         ,"#####.##.#.##.###.##"
--         ,"..#####..#.#########"
--         ,"####################"
--         ,"#.####....###.#.#.##"
--         ,"##.#################"
--         ,"#####.##.###..####.."
--         ,"..######..##.#######"
--         ,"####.##.####...##..#"
--         ,".#####..#.######.###"
--         ,"##...#.##########..."
--         ,"#.##########.#######"
--         ,".####.#.###.###.#.##"
--         ,"....##.##.###..#####"
--         ,".#.#.###########.###"
--         ,"#.#.#.#####.####.###"
--         ,"###.##.####.##.#..##"
--         ]
-- test4 = numBestByVisible map4 == 210

-- map5base = (8,3) :: (Double, Double)
-- map5 = parseInput $ unlines
--         [".#....#####...#.."
--         ,"##...##.#####..##"
--         ,"##...#...#.#####."
--         ,"..#.....#...###.."
--         ,"..#.#.....#....##"
--         ]
