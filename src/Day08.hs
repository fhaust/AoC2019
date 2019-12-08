
import           Data.Function
import           Data.List
import           Data.List.Split

width = 25
height = 6
input = splitLayers width height . init <$> readFile "inputs/day08.txt"


splitLayers w h = map concat . chunksOf h . chunksOf w


numOf x = length . filter (==x)


part1 layers = numOf '1' thatLayer * numOf '2' thatLayer
  where
    thatLayer = minimumBy (compare `on` numOf '0') $ layers


combineLayers layers = foldl1' (zipWith combine) layers
  where
    combine '2' y = y
    combine  x  _ = x

showImage w = chunksOf w . map go
  where go '0' = ' '
        go '1' = '#'

part2 = showImage width . combineLayers


main = do

  layers <- input

  putStrLn $ "Result 1: " ++ show (part1 layers)

  putStrLn "Result 2:"
  mapM_ putStrLn . part2 $ layers




