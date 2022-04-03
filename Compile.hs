-- Functions for "compiling" the scanned CESIL code lines into an
-- executable representation.

module Compile
  ( makeJumpTable
  , compile
  ) where

import qualified Data.Map as Map
import Data.Char
import Text.Read

import ScanSource


-- Define some types
type Address = Integer
type JumpTable = Map.Map String Address

-- Create the jump table
-- TODO does this belong here?
makeJumpTable :: [CodeLine] -> JumpTable
makeJumpTable cl = foldr jtBuilder Map.empty $ zip [0 :: Address ..] cl
  where jtBuilder (_, CodeLine _ "" _ _) jt = jt
        jtBuilder (n, CodeLine lineNo label _ _) jt = Map.insert label n jt


-- Define type for a compiled instruction
data Instruction = Instruction LineNo OpCode Operand deriving (Show)

-- Define instructions
data OpCode = IN
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
data Operand = NoOperand
             | ValueOperand (Either Integer String)
             | SymbolOperand String
             | AddrOperand Address
             | TextOperand String
             deriving (Show)

-- Relate instruction spelling, opcode, and operand types
instructionDefs = Map.fromList
  [("IN", (IN, parseNoOperand))
  ,("OUT", (OUT, parseNoOperand))
  ,("LINE", (LINE, parseNoOperand))
  ,("PRINT", (PRINT, parseTextOperand))
  ,("HALT", (HALT, parseNoOperand))
  ,("LOAD", (LOAD, parseValueOperand))
  ,("STORE", (STORE, parseSymbolOperand))
  ,("ADD", (ADD, parseValueOperand))
  ,("SUBTRACT", (SUBTRACT, parseValueOperand))
  ,("MULTIPLY", (MULTIPLY, parseValueOperand))
  ,("DIVIDE", (DIVIDE, parseValueOperand))
  ,("JUMP", (JUMP, parseAddrOperand))
  ,("JIZERO", (JIZERO, parseAddrOperand))
  ,("JINEG", (JINEG, parseAddrOperand))
  ]


-- Compile the parsed codelines to instructions (or errors)
compile :: [CodeLine] -> JumpTable -> [Either String Instruction]
compile cl jt = foldr (\ c prog -> compIns c jt : prog) [] cl


-- Compile a single instruction
-- TODO improve error reporting
compIns :: CodeLine -> JumpTable -> Either String Instruction
compIns (CodeLine lineNo _ "" _) jumpTable =
  Right $ Instruction lineNo NoOp NoOperand
compIns (CodeLine lineNo _ instr pstr) jumpTable =
  case Map.lookup instr instructionDefs of
    Just (opcode, parser) -> case parser pstr jumpTable of
      Right opa -> Right $ Instruction lineNo opcode opa
      Left err -> Left $ "Line " ++ show lineNo ++ ": " ++ err
    Nothing -> Left $ "Line " ++ show lineNo ++ ": Unknown instruction: " ++ instr


-- Define operand parsers

parseNoOperand :: String -> JumpTable -> Either String Operand
parseNoOperand "" _ = Right NoOperand
parseNoOperand _ _ = Left "Unexpected parameter"


parseValueOperand :: String -> JumpTable -> Either String Operand
parseValueOperand "" _ = Left "Missing parameter"
parseValueOperand p _
  | isDigit $ head p = case readMaybe p :: Maybe Integer of
      Just n ->  Right $ ValueOperand $ Left n
      Nothing -> Left $ "Invalid number: " ++ p
  | otherwise = if isValidSymbolName p
                then Right $ ValueOperand $ Right p
                else Left "Invalid symbol name"


parseSymbolOperand :: String -> JumpTable -> Either String Operand
parseSymbolOperand "" _ = Left "Missing parameter"
parseSymbolOperand p _ = if isValidSymbolName p
                         then Right $ SymbolOperand p
                         else Left "Invalid symbol name"


parseAddrOperand :: String -> JumpTable -> Either String Operand
parseAddrOperand "" _ = Left "Missing parameter"
parseAddrOperand p jt = case Map.lookup p jt of
  Just n -> Right $ AddrOperand n
  Nothing -> Left $ "Undefined jump destination: " ++ p


parseTextOperand :: String -> JumpTable -> Either String Operand
parseTextOperand s _
  | length s < 2 = Left "Invalid text"
  | head s /= '"' || last s /= '"' = Left "Quote error"
  | otherwise = Right $ TextOperand $ reverse $ drop 1 $ reverse $ drop 1 s


isValidSymbolName :: String -> Bool
isValidSymbolName "" = False
isValidSymbolName s
  | not . isAlpha . head $ s = False
  | all (\ l -> (isAlphaNum l) || (l == '_')) s = True
  | otherwise = False
