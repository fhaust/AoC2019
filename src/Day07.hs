
import           IntcodeComputer

import           Data.Foldable
import           Data.List

-- | read and "compile" the input program
input = compile <$> readFile "inputs/day07.txt"

-- PART 1

-- | run every "amplifier" in a row and pipe each output to the next
runAmplifiers prog phases = foldl' (\x p -> run2result [p,x] prog) 0 phases

-- | find the permutation of "phases" that results in the highest "output"
part1 prog = maximum [ runAmplifiers prog phases | phases <- permutations [0..4] ]

-- PART 2

-- | run all "amplifiers" again ... but this time pipe the output of 
--   the last one to the first one
runAmplifierLoop prog [p0,p1,p2,p3,p4] = last r4
  where
    r0 = run2results (p0:0:r4) prog
    r1 = run2results (p1:r0) prog
    r2 = run2results (p2:r1) prog
    r3 = run2results (p3:r2) prog
    r4 = run2results (p4:r3) prog

-- | again find the permutation that yields the highest output
part2 prog = maximum [ runAmplifierLoop prog phases | phases <- permutations [5..9] ]

-- | main program runs everything
main = do

  prog <- input

  putStrLn $ "All Tests Pass: " ++ show tests
  putStrLn $ "Result1: " ++ show (part1 prog)
  putStrLn $ "Result2: " ++ show (part2 prog)















-- Examples:

tests = all (==True) [test1, test2, test3, test4, test5]

prog1 = compile "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
test1 = runAmplifiers prog1 [4,3,2,1,0] == 43210

prog2 = compile "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
test2 = runAmplifiers prog2 [0,1,2,3,4] == 54321

prog3 = compile "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
test3 = runAmplifiers prog3 [1,0,4,3,2] == 65210



prog4 = compile $ "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,"
               ++ "27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
phases4 = [9,8,7,6,5]
test4 = runAmplifierLoop prog4 phases4 == 139629729

prog5 = compile $ "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,"
               ++ "-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,"
               ++ "53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
phases5 = [9,7,8,5,6]
test5 = runAmplifierLoop prog5 phases5 == 18216
