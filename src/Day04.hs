
import           Data.Char
import           Data.List

-- | input and input as single digits
input = [372304..847060]
input' = map (map digitToInt . show) $ input


-- | are all digits monotonically increasing
isIncreasing xs = all (==True) $ zipWith (<=) xs (tail xs)

-- | does the password contain double digits
hasDoubleDigit xs = any (==True) $ zipWith (==) xs (tail xs)

-- | does the password contain double digits but not triple or more digits
hasDoubleDigit' xs = (2 `elem`) . map length . group $ xs



-- | result part 1
filter1 x = isIncreasing x && hasDoubleDigit x
result1 = length . filter filter1 $ input'

-- | result part 2
filter2 x = isIncreasing x && hasDoubleDigit' x
result2 = length . filter filter2 $ input'


main = do

  putStrLn $ "Result 1: " ++ show result1
  putStrLn $ "Result 2: " ++ show result2


