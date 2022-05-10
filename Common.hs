-- Common definitions used in compilation and execution phases.
module Common
  ( LineNo
  , Address
  , Instruction(..)
  , OpCode(..)
  , Operand(..)
  ) where

-- Define some types
type LineNo = Integer

type Address = Integer

data Instruction =
  Instruction LineNo OpCode Operand
  deriving (Show)

-- Define instructions
data OpCode
  = IN
  | OUT
  | LINE
  | PRINT
  | HALT
  | LOAD
  | STORE
  | ADD
  | SUBTRACT
  | MULTIPLY
  | DIVIDE
  | JUMP
  | JIZERO
  | JINEG
  | NoOp
  deriving (Show)

-- Define operand types
data Operand
  = NoOperand
  | ValueOperand (Either Integer String)
  | SymbolOperand String
  | AddrOperand Address
  | TextOperand String
  deriving (Show)
