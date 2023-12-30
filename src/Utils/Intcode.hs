module Utils.Intcode(
  Machine (..),
  Address (..),
  Memory (..),
  setAll,
  (@),
  machineFromList,
  runProgram,
  instructionAtAddress,
) where

import           Control.Lens (element, (.~))

newtype Memory = Memory [Int] deriving (Show)
newtype Address = Address Int deriving (Show)
data Instruction = Add Operand Operand Address
                 | Multiply Operand Operand Address
                 | Input Address
                 | Output Operand
                 | JumpIfTrue Operand Operand
                 | JumpIfFalse Operand Operand
                 | LessThan Operand Operand Address
                 | Equals Operand Operand Address
                 | Halt deriving (Show)
data Operand = Pointer Address
             | Immediate Int deriving (Show)

data Machine = Machine { getMemory :: Memory
                       , getIp     :: Address
                       , getInput  :: [Int]
                       , getOutput :: [Int]
                       , isHalted  :: Bool
                       } deriving (Show)

machineFromList :: [Int] -> Machine
machineFromList m = Machine { getMemory = Memory m
                            , getIp = Address 0
                            , getInput = []
                            , getOutput = []
                            , isHalted = False
                            }

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

instructionSize :: Instruction -> Int
instructionSize (Add {})         = 4
instructionSize (Multiply {})    = 4
instructionSize (Input {})       = 2
instructionSize (Output {})      = 2
instructionSize (JumpIfTrue {})  = 3
instructionSize (JumpIfFalse {}) = 3
instructionSize (LessThan {})    = 4
instructionSize (Equals {})      = 4
instructionSize Halt             = 1

getValue :: Memory -> Operand -> Int
getValue m (Pointer a)   = m @ a
getValue _ (Immediate v) = v

math :: Memory -> Operand -> Operand -> Address -> (Int -> Int -> Int) -> Memory
math m x y a f = let
  v = f (getValue m x) (getValue m y)
  in set m a v

runInstruction :: Machine -> Instruction -> Machine
runInstruction m Halt = m { isHalted = True }
runInstruction m i@(Add a b r) =
  m { getMemory = math (getMemory m) a b r (+)
    , getIp = getIp m @+ instructionSize i
    }
runInstruction m i@(Multiply a b r) =
  m { getMemory = math (getMemory m) a b r (*)
    , getIp = getIp m @+ instructionSize i
    }
runInstruction m i@(Input a) =
  m { getMemory = set (getMemory m) a (head $ getInput m)
    , getIp = getIp m @+ instructionSize i
    , getInput = tail $ getInput m
    }
runInstruction m i@(Output a) = let
  v = getValue (getMemory m) a
  in m { getIp = getIp m @+ instructionSize i
       , getOutput = v:getOutput m }
runInstruction m i@(JumpIfTrue a b) = let
  testValue = getValue (getMemory m) a
  ipIfJump = Address $ getValue (getMemory m) b
  ipIfNotJump = getIp m @+ instructionSize i
  in m { getIp = if testValue /= 0 then ipIfJump else ipIfNotJump }
runInstruction m i@(JumpIfFalse a b) = let
  testValue = getValue (getMemory m) a
  ipIfJump = Address $ getValue (getMemory m) b
  ipIfNotJump = getIp m @+ instructionSize i
  in m { getIp = if testValue == 0 then ipIfJump else ipIfNotJump }
runInstruction m i@(LessThan a b r) =
  m { getMemory = math (getMemory m) a b r (\x y -> if x < y then 1 else 0)
    , getIp = getIp m @+ instructionSize i }
runInstruction m i@(Equals a b r) =
  m { getMemory = math (getMemory m) a b r (\x y -> if x == y then 1 else 0)
    , getIp = getIp m @+ instructionSize i }

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

addPadding :: String -> String
addPadding opcode = let
  l = length opcode
  omittedCount = 5 - l
  missingZeros = replicate omittedCount '0'
  in missingZeros ++ opcode

extractOperands :: Memory -> Address -> (String, [Operand])
extractOperands m a = let
  code = addPadding $ show $ m @ a
  cMode = slice 0 0 code
  bMode = slice 1 1 code
  aMode = slice 2 2 code
  opcode = slice 3 4 code
  operand mode aa = case mode of
    "0" -> Pointer (Address (m @ aa))
    "1" -> Immediate (m @ aa)
    c   -> error $ "Unexpected mode=" ++ show c
  in ( opcode, [ operand aMode (a @+ 1)
               , operand bMode (a @+ 2)
               , operand cMode (a @+ 3) ])

extractAddress :: Operand -> Address
extractAddress (Pointer p) = p
extractAddress o = error $ "Can't extract address from operand=" ++ show o

instructionAtAddress :: Memory -> Address -> Instruction
instructionAtAddress m a = let
  (opcode, ops) = extractOperands m a
  in case opcode of
    "01" -> Add (ops !! 0) (ops !! 1) (extractAddress (ops !! 2))
    "02" -> Multiply (ops !! 0) (ops !! 1) (extractAddress (ops !! 2))
    "03" -> Input (extractAddress (head ops))
    "04" -> Output (head ops)
    "05" -> JumpIfTrue (ops !! 0) (ops !! 1)
    "06" -> JumpIfFalse (ops !! 0) (ops !! 1)
    "07" -> LessThan (ops !! 0) (ops !! 1) (extractAddress (ops !! 2))
    "08" -> Equals (ops !! 0) (ops !! 1) (extractAddress (ops !! 2))
    "99" -> Halt
    x  -> error $ "Unexpected opcode=" ++ show x ++ " a=" ++ show a ++ " m=" ++ show m

runProgram :: Machine -> Machine
runProgram m = let
  ip = getIp m
  memory = getMemory m
  instruction = instructionAtAddress memory ip
  m' = runInstruction m instruction
  in if isHalted m
     then m
     else runProgram m'
