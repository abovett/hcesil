-- Common definitions used in compilation and execution phases.
module Common
  ( Params(..)
  , LineNo
  , Address
  , CodeLine(..)
  , Instruction(..)
  , OpCode(..)
  , Operand(..)
  ) where

-- All program parameters
data Params =
  Params
    { filename :: Maybe String
    , help :: Bool
    , version :: Bool
    , countSteps :: Bool
    , maxSteps :: Maybe Integer
    , trace :: Bool
    }
  deriving (Show)

-- Define some types
type LineNo = Integer

type Address = Integer

-- A scanned line of source comprising line number, label, instruction
-- and operand.
data CodeLine =
  CodeLine LineNo String String String
  deriving (Show)

-- Define a "compiled" instruction
data Instruction =
  Instruction CodeLine OpCode Operand
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
  deriving (Show, Eq)

-- Define operand types
data Operand
  = NoOperand
  | ValueOperand (Either Integer String)
  | SymbolOperand String
  | AddrOperand Address
  | TextOperand String
  deriving (Show)
