
requiredFuel1 x = x `div` 3 - 2
requiredFuel2 = sum . takeWhile (>0) . drop 1 . iterate (max 0 . requiredFuel1)

-- probably more readable than the equivalent requiredFuel2
requiredFuel3 x | x' > 0     = x' + requiredFuel3 x'
                | otherwise  = x'
  where x' = max 0 (requiredFuel1 x)

main = do

  input <- map read . lines <$> readFile "inputs/day01.txt" :: IO [Int]

  let result1 = sum . map requiredFuel1 $ input
  let result2 = sum . map requiredFuel2 $ input

  print $ "Result 1: " ++ show result1
  print $ "Result 2: " ++ show result2
