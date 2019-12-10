{-# LANGUAGE ViewPatterns #-}


module IntcodeComputer (step, compile, run2halt, run2result, run2results, IntCode(..), initState)
where

import qualified Data.Sequence as S
import           Data.Sequence (Seq (..))

import           Data.Foldable
import           Data.List.Split

import           Text.Printf

import           Debug.Trace

import           Data.Char

newtype IntCode = IC { unIC :: [Int] } deriving Show

type InstructionPointer = Int
type RelativeBase = Int
type Inputs = [Int]
type Outputs = [Int]
type Memory = Seq Int

type State = (InstructionPointer, RelativeBase, Inputs, Outputs, Memory)

step :: State -> State
step (p, rb, inp, out, cs) = go (trace (show. S.take 4 $ S.drop p cs) (S.drop p cs))
  where

    -- halt cmd
    go (parse -> ([_,_,_,9,9], _      )) = (-1, rb, inp, out, cs)

    -- read input
    go (parse -> ([_,_,x,_,3], (c:_)  )) = (p + 2, rb, tail inp, out,            write (wMode x c) (head inp))
    -- write output
    go (parse -> ([_,_,x,_,4], (c:_)  )) = (p + 2, rb, inp,      mode x c : out, cs)


    -- add and multiply
    go (parse -> ([z,y,x,0,1], (a:b:c:_))) = (p + 4, rb, inp, out, write (wMode z c) (mode x a + mode y b))
    go (parse -> ([z,y,x,0,2], (a:b:c:_))) = (p + 4, rb, inp, out, write (wMode z c) (mode x a * mode y b))

    -- jump instructions
    go (parse -> ([_,y,x,0,5], (a:b:_))) = (if mode x a /= 0 then mode y b else p + 3, rb, inp, out, cs)
    go (parse -> ([_,y,x,0,6], (a:b:_))) = (if mode x a == 0 then mode y b else p + 3, rb, inp, out, cs)

    -- comparisons
    go (parse -> ([z,y,x,0,7], (a:b:c:_))) = (p + 4, rb, inp, out, write (wMode z c) (if mode x a < mode y b then 1 else 0))
    go (parse -> ([z,y,x,0,8], (a:b:c:_))) = (p + 4, rb, inp, out, write (wMode z c) (if mode x a == mode y b then 1 else 0))

    -- modify relative base
    go (parse -> ([_,_,x,0,9], (a:_)))     = (p + 2, rb + mode x a, inp, out, cs)

    -- halt if something goes wrong
    go _                                 = error $ "received invalid opcode: " ++ show (p, inp, out, cs)

    -- utils

    -- write value x to address t
    write t x | t < S.length cs = S.update t x cs
              | otherwise       = S.update t x (cs <> S.replicate (t*2) 0)
    -- read from address
    reg     t | t < S.length cs = cs `S.index` t
              | otherwise       = 0
    -- immediate or position mode
    mode 0 a = reg a
    mode 1 a = a
    mode 2 a = reg (rb + a)

    read m p | m == 0 
             | p < S.length cs = cs `S.index` p'
             | otherwise       = 0
      where p' = case m of 
                   0 -> p
                   1 -> 
                   2 -> rb + a

    wMode 0 a = a
    wMode 2 a = rb + a

-- more or less the whole parsing logic
parse ((digits -> op) :<| ps) = (op, toList ps)

-- split "int code" into seperate digits
digits z = [a,b,c,d,e]
  where
    (y,e) = z `divMod` 10
    (x,d) = y `divMod` 10
    (w,c) = x `divMod` 10
    (v,b) = w `divMod` 10
    (_,a) = v `divMod` 10


-- | run a program until it has reached a 99 opcode (or just crashed)
-- run2halt :: [Integer] -> IntCode -> ([Integer], IntCode)
run2halt input prog = (\(_,_,_,b,c) -> (reverse b, IC . toList $ c))
                    . last
                    . takeWhile (\(p,_,_,_,_) -> p /= (-1))
                    . iterate step
                    $ initState input prog

-- | simple wrapper for run2halt that runs a program with multiple parameters
--   and returns all results
-- run2results :: [Int] -> IntCode -> [Int]
run2results input prog = fst $ run2halt input prog

-- | simple wrapper for run2halt that runs a program and returns its first result
-- run2result :: [Int] -> IntCode -> Int
run2result input prog = head $ run2results input prog

initState input (IC prog) = (0, 0, input, [], S.fromList prog)


-- | "compile" inputs into IntCode
compile :: String -> IntCode
compile = IC .  map read . splitOn ","
