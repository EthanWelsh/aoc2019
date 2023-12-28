module Utils.Intcode(
  Memory,
  Address,
  Instruction,
  address,
  memoryFromList,
  get,
  set,
  setAll,
  (@),
  (@+),
  runInstruction,
  setInputs,
  instructionAtAddress,
  runProgram,
  runProgramWithInputs
) where

import           Control.Lens         (element, (.~))
import           Data.Tuple.Extra     (uncurry3)

newtype Memory = Memory [Int] deriving (Show)
newtype Address = Address Int deriving (Show, Num)
data Instruction = Add Address Address Address
                 | Multiply Address Address Address
                 | End deriving (Show)

memoryFromList :: [Int] -> Memory
memoryFromList = Memory

address :: Int -> Address
address n = Address n

instructionSize :: Instruction -> Int
instructionSize (Add {})      = 4
instructionSize (Multiply {}) = 4
instructionSize End           = 1

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

runInstruction :: Memory -> Instruction -> Memory
runInstruction m End              = m
runInstruction m (Add a b r)      = set m r (m @ a + m @ b)
runInstruction m (Multiply a b r) = set m r (m @ a * m @ b)

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
    99 -> End
    x -> error $ "Unexpected opcode=" ++ show x

isEnd :: Instruction -> Bool
isEnd End = True
isEnd _   = False

runProgram :: Memory -> Address -> Memory
runProgram m a = let
  instruction = instructionAtAddress m a
  nextA = a @+ instructionSize instruction
  result = runInstruction m instruction
  in if isEnd instruction
     then m
     else runProgram result nextA

runProgramWithInputs :: Memory -> (Int, Int) -> Int
runProgramWithInputs m inputs = let
  mm = setInputs m inputs
  in runProgram mm (Address 0) @ 0
