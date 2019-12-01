
requiredFuel1 x = x `div` 3 - 2
requiredFuel2 = sum . takeWhile (>0) . drop 1 . iterate (max 0 . requiredFuel1)

main = do

  input <- map read . lines <$> readFile "inputs/day01.txt" :: IO [Int]

  let result1 = sum . map requiredFuel1 $ input
  let result2 = sum . map requiredFuel2 $ input

  print $ "Result 1: " ++ show result1
  print $ "Result 2: " ++ show result2
