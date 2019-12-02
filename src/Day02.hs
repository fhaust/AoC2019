
import qualified Data.Sequence as S

import           Data.List
import           Data.List.Split


-- | run one step in the program (this could be should be much nicer using sequences views)
step (p,cs) | a == 1  = (p+4, S.update d ((cs `S.index` b) + (cs `S.index` c)) cs)
            | a == 2  = (p+4, S.update d ((cs `S.index` b) * (cs `S.index` c)) cs)
            | a == 99 = (-1,  cs)
  where
    a = cs `S.index` (p+0)
    b = cs `S.index` (p+1)
    c = cs `S.index` (p+2)
    d = cs `S.index` (p+3)

-- | run the program, but we are only interested in the very first field in the end
runProgram cs = (`S.index` 0) . snd . last . takeWhile (\(p,_) -> p >= 0 ) $ iterate step (0, cs)

-- | run program, but change field 1 and 2
runProgramWith a b cs = runProgram $ S.update 1 a . S.update 2 b $ cs

main = do

  input <- readFile "inputs/day02.txt"
  let cmds = S.fromList .  map read . splitOn "," . init $ input :: S.Seq Int

  -- part 1 ... run the program with changed parameters
  putStrLn $ "Result 1: " ++ show (runProgramWith 12 2 cmds)


  -- part 2 ... finding two more integers that result in 19690720 ... brute force it!
  let target = 19690720
      (a,_) = last $ takeWhile (\(_,r) -> r < target) [ (a, runProgramWith a 0 cmds) | a <- [0..] ]
      (Just (b,_)) = find (\(_,r) -> r == target) [ (b, runProgramWith a b cmds) | b <- [0..] ]

  putStrLn $ "Result 2: " ++ show (100 * a + b)

