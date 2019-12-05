


import qualified Data.Sequence as S
import           Data.Sequence (Seq (..))

import           Data.List
import           Data.List.Split

type Reg = Int

data Par = Reg Int | Val Int deriving Show

data OpCode = Read          Reg
            | Write         Reg
            | Add   Par Par Reg
            | Mul   Par Par Reg
            | End
  deriving Show

parseOpCodes = go
  where
    -- end op code
    go []                       = []
    go ("99" :            cmds) = End         : go cmds

    -- simple op codes
    go ("1" : a : b : c : cmds) = AddRR a b c : go cmds
    go ("2" : a : b : c : cmds) = MulRR a b c : go cmds
    go ("3" :         c : cmds) = Read      c : go cmds
    go ("4" :         c : cmds) = Write     c : go cmds

    -- intermediate op codes
    go ("01" : a : b : c : cmds) = AddRR a b c : go cmds
    go ("02" : a : b : c : cmds) = MulRR a b c : go cmds





-- step (p, cs) = go (S.drop p cs)
--   where
--     go (99 :<| _)                  = (-1, cs)
--     go (1 :<| b :<| c :<| d :<| _) = (p+4, S.update d ((cs `S.index` b) + (cs `S.index` c)) cs)
--     go (2 :<| b :<| c :<| d :<| _) = (p+4, S.update d ((cs `S.index` b) * (cs `S.index` c)) cs)
--     go (3 :<| b :<| _) = (p+4, S.update d ((cs `S.index` b) * (cs `S.index` c)) cs)

-- step (p,cs) | a == 1  = (p+4, S.update d ((cs `S.index` b) + (cs `S.index` c)) cs)
--             | a == 2  = (p+4, S.update d ((cs `S.index` b) * (cs `S.index` c)) cs)
--             | a == 99 = (-1,  cs)
--   where
--     a = cs `S.index` (p+0)
--     b = cs `S.index` (p+1)
--     c = cs `S.index` (p+2)
--     d = cs `S.index` (p+3)
