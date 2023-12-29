module Utils.Intcode(
  Machine (..),
  Address (..),
  (@),
  setInputs,
  machineFromList,
  runProgram,
) where

import           Control.Lens         (element, (.~))
import           Data.Tuple.Extra     (uncurry3)

newtype Memory = Memory [Int] deriving (Show)
newtype Address = Address Int deriving (Show, Num)
data Instruction = Add Address Address Address
                 | Multiply Address Address Address
                 | Halt deriving (Show)

data Machine = Machine { getMemory :: Memory
                       , getIp :: Address
                       , getInput :: [Int]
                       , getOutput :: [Int]
                       , isHalted :: Bool
                       } deriving (Show)

machineFromList :: [Int] -> Machine
machineFromList m = Machine { getMemory = Memory m
                            , getIp = Address 0
                            , getInput = []
                            , getOutput = []
                            , isHalted = False
                            }

instructionSize :: Instruction -> Int
instructionSize (Add {})      = 4
instructionSize (Multiply {}) = 4
instructionSize Halt           = 1

get :: Memory -> Address -> Int
get (Memory m) (Address a) = m !! a

set :: Memory -> Address -> Int -> Memory
set (Memory m) (Address a) v = Memory $ (element a .~ v) m

setAll :: Memory -> [(Address, Int)] -> Memory
setAll = foldl (\m (a, v) -> set m a v)

(@) :: Memory -> Address -> Int
(@) = get

(@+) :: Address -> Int -> Address
(@+) (Address a) offset = Address $ a + offset

runInstruction :: Machine -> Instruction -> Machine
runInstruction m Halt = m { isHalted = True }
runInstruction m i@(Add a b r) = let
  mem = getMemory m
  mem' = set mem r (mem @ a + mem @ b)
  ip' = getIp m @+ instructionSize i
  in m { getMemory = mem'
       , getIp = ip' }
runInstruction m i@(Multiply a b r) = let
  mem = getMemory m
  mem' = set mem r (mem @ a * mem @ b)
  ip' = getIp m @+ instructionSize i
  in m { getMemory = mem'
       , getIp = ip' }

setInputs :: Memory -> (Int, Int) -> Memory
setInputs m (verb, noun) = setAll m [(1, verb), (2, noun)]

next3 :: Memory -> Address -> (Address, Address, Address)
next3 m a = (Address $ m @ (a @+ 1), Address $ m @ (a @+ 2), Address $ m @ (a @+ 3))

instructionAtAddress :: Memory -> Address -> Instruction
instructionAtAddress m a = let
  opcode = m @ a
  in case opcode of
    1  -> uncurry3 Add (next3 m a)
    2  -> uncurry3 Multiply (next3 m a)
    99 -> Halt
    x -> error $ "Unexpected opcode=" ++ show x

runProgram :: Machine -> Machine
runProgram m = let
  ip = getIp m
  memory = getMemory m
  instruction = instructionAtAddress memory ip
  m' = runInstruction m instruction
  in if isHalted m
     then m
     else runProgram m'
