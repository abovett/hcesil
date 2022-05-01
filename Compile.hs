-- Functions for "compiling" the scanned CESIL code lines into an
-- executable representation.

module Compile
  ( compile
  , Instruction
  ) where

import qualified Data.Map as Map
import Data.Char
import Text.Read
import Data.List
import Data.Either

import ScanSource


-- Define some types
type Address = Integer
type JumpTable = Map.Map String Address
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


-- "Compile" the program
compile :: String -> Either String ([Instruction], [Integer])
compile source =
  let (codeLines, dataLines) = scanSource source
      jumpTable = makeJumpTable codeLines
      (pErrors, code) = partitionEithers $ compileCode codeLines jumpTable
      (dErrors, dataVals) = partitionEithers $ getDataValues dataLines
      errors = pErrors ++ dErrors
  in if null errors
     then Right (code, dataVals)
     else let s = concat $ intersperse "\n" errors
          in Left $ "Compilation failed.\n" ++ s


-- Create the jump table
makeJumpTable :: [CodeLine] -> JumpTable
makeJumpTable cl = foldr jtBuilder Map.empty $ zip [0 :: Address ..] cl
  where jtBuilder (_, CodeLine _ "" _ _) jt = jt
        jtBuilder (n, CodeLine lineNo label _ _) jt = Map.insert label n jt


-- Compile the parsed codelines to instructions (or errors)
compileCode :: [CodeLine] -> JumpTable -> [Either String Instruction]
compileCode cl jt = foldr (\ c code -> compileInst c jt : code) [] cl


-- Compile a single instruction
compileInst :: CodeLine -> JumpTable -> Either String Instruction
compileInst (CodeLine lineNo _ "" _) jumpTable =
  Right $ Instruction lineNo NoOp NoOperand
compileInst (CodeLine lineNo _ instr pstr) jumpTable =
  case Map.lookup instr instructionDefs of
    Just (opcode, parser) -> case parser pstr jumpTable of
      Right opa -> Right $ Instruction lineNo opcode opa
      Left err -> composeError err
    Nothing -> composeError $ "Unknown instruction: " ++ instr
  where composeError err = Left $ "Line " ++ show lineNo ++ ": " ++ err


-- Define operand parsers

parseNoOperand :: String -> JumpTable -> Either String Operand
parseNoOperand "" _ = Right NoOperand
parseNoOperand p _ = Left $ "Unexpected operand: " ++ p


parseValueOperand :: String -> JumpTable -> Either String Operand
parseValueOperand "" _ = Left "Missing parameter"
parseValueOperand p _
  | isDigit $ head p = case readMaybe p :: Maybe Integer of
      Just n ->  Right $ ValueOperand $ Left n
      Nothing -> Left $ "Invalid number: " ++ p
  | otherwise = if isValidSymbolName p
                then Right $ ValueOperand $ Right p
                else Left $ "Invalid symbol name: " ++ p


parseSymbolOperand :: String -> JumpTable -> Either String Operand
parseSymbolOperand "" _ = Left "Missing parameter"
parseSymbolOperand p _ = if isValidSymbolName p
                         then Right $ SymbolOperand p
                         else Left $ "Invalid symbol name: " ++ p


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


-- Convert data lines to values
getDataValues :: [DataLine] -> [Either String Integer]
getDataValues dl = foldr (\ d dVals -> parseData d : dVals) [] dl
  where parseData (DataLine lineNo s) = case readMaybe s :: Maybe Integer of
          Just v -> Right v
          Nothing -> Left $ "Line " ++ show lineNo ++ ": " ++ "Invalid data: " ++ s
