{-# LANGUAGE ViewPatterns #-}


module IntcodeComputer where

import qualified Data.Sequence as S
import           Data.Sequence (Seq (..))

import           Data.Foldable
import           Data.List.Split

import           Text.Printf

import           Data.Char

newtype IntCode = IC { unIC :: [Int] } deriving Show

step (p, inp, out, cs) = go (S.drop p cs)
  where

    -- halt cmd
    go (parse -> ([_,_,_,9,9], _      )) = (-1, inp, out, cs)

    -- read and write input
    go (parse -> ([_,_,_,_,3], (c:_)  )) = (p + 2, tail inp, out,            write c (head inp))
    go (parse -> ([_,_,x,_,4], (c:_)  )) = (p + 2, inp,      out ++ [mode x c], cs)


    -- add and multiply
    go (parse -> ([_,y,x,0,1], [a,b,c])) = (p + 4, inp, out, write c (mode x a + mode y b))
    go (parse -> ([_,y,x,0,2], [a,b,c])) = (p + 4, inp, out, write c (mode x a * mode y b))

    -- jump instructions
    go (parse -> ([_,y,x,0,5], (a:b:_))) = (if mode x a /= 0 then mode y b else p + 3, inp, out, cs)
    go (parse -> ([_,y,x,0,6], (a:b:_))) = (if mode x a == 0 then mode y b else p + 3, inp, out, cs)

    -- comparisons
    go (parse -> ([_,y,x,0,7], [a,b,c])) = (p + 4, inp, out, write c (if mode x a < mode y b then 1 else 0))
    go (parse -> ([_,y,x,0,8], [a,b,c])) = (p + 4, inp, out, write c (if mode x a == mode y b then 1 else 0))

    -- halt if something goes wrong
    go _                                 = error $ "received invalid opcode: " ++ show (p, inp, out, cs)

    -- utils

    -- write value x to address t
    write t x = S.update t x cs
    -- read from address
    reg     t = cs `S.index` t
    -- immediate or position mode
    mode 0 a = reg a
    mode 1 a = a

-- more or less the whole parsing logic
parse (S.take 4 -> ((digits -> op) :<| ps)) = (op, toList ps)

-- split "int code" into seperate digits
digits = map digitToInt . printf "%05d"


-- | run a program until it has reached a 99 opcode (or just crashed)
run2halt :: [Int] -> IntCode -> ([Int], IntCode)
run2halt input (IC prog) = (\(_,_,b,c) -> (b, IC . toList $ c))
                         . last
                         . takeWhile (\(p,_,_,_) -> p /= (-1))
                         . iterate step
                         $ (0, input, [], S.fromList prog)

-- | simple wrapper for run2halt that runs a program with multiple parameters
--   and returns all results
run2results :: [Int] -> IntCode -> [Int]
run2results input prog = fst $ run2halt input prog

-- | simple wrapper for run2halt that runs a program and returns its first result
run2result :: [Int] -> IntCode -> Int
run2result input prog = head $ run2results input prog



-- | "compile" inputs into IntCode
compile :: String -> IntCode
compile = IC .  map read . splitOn ","
