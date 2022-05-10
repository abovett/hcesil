-- Functions for executing the compiled CESIL code
module Execute
  ( runProgram
  ) where

import Control.Monad.Except
import Data.Array
import qualified Data.Map as Map
import Text.Printf

import Common
  ( Address
  , Instruction(..)
  , LineNo
  , OpCode(..)
  , Operand(..)
  , Params(..)
  )

-- Define the computer state
data Computer =
  Computer
    { program :: Array Integer Instruction
    , dataVals :: [Integer]
    , ram :: Map.Map String Integer
    , acc :: Integer
    , pc :: Integer
    , steps :: Integer
    , halted :: Bool
    }

-- Initialise the computer and run the program
runProgram ::
     Array Integer Instruction -> [Integer] -> Params -> ExceptT String IO ()
runProgram pr dv pars = do
  let comp =
        Computer
          { program = pr
          , dataVals = dv
          , ram = Map.empty
          , acc = 0
          , pc = 0
          , steps = 0
          , halted = False
          }
  comp' <- execute comp pars
  if countSteps pars
    then liftIO . putStrLn $ "Steps executed: " ++ (show $ steps comp')
    else return ()

-- Main execution "loop"
execute :: Computer -> Params -> ExceptT String IO Computer
execute comp pars = do
  liftEither $ checkPC comp
  (comp', output) <- liftEither $ step comp
  liftIO $ putStr output
  case () of
    _
      | halted comp' -> return comp'
      | Just (steps comp') == maxSteps pars -> do
        liftIO $
          putStrLn $
          "Execution halted after " ++ (show $ steps comp') ++ " steps."
        return comp'
      | otherwise -> execute comp' pars

-- Check program counter is in range. It shouldn't be possible for it
-- to go out of range unless compilation has gone wrong somehow, but
-- check just in case.
checkPC :: Computer -> Either String ()
checkPC comp =
  if pc comp >= (toInteger . length . program $ comp) || pc comp < 0
    then Left $ "PC OUT OF RANGE: " ++ (show $ pc comp) ++ "\n"
    else Right ()

-- Execute a single step/cycle
step :: Computer -> Either String (Computer, String)
step comp = do
  let Instruction lineNo opCode operand =
        program comp ! (fromIntegral . pc $ comp)
      comp' = comp {pc = pc comp + 1, steps = steps comp + 1}
  case opCode of
    IN ->
      if null $ dataVals comp
        then Left $ "Data exhausted at line " ++ show lineNo ++ "\n"
        else let a:dv = dataVals comp
              in Right (comp {acc = a, dataVals = dv, pc = pc comp + 1}, "")
    OUT -> Right (comp', printf "%8d" $ acc comp)
    LINE -> Right (comp', "\n")
    PRINT ->
      let TextOperand s = operand
       in Right (comp', s)
    HALT -> Right (comp' {halted = True}, "")
    LOAD -> do
      n <- getVal operand comp' lineNo
      Right (comp' {acc = n}, "")
    STORE ->
      let SymbolOperand s = operand
          ram' = Map.insert s (acc comp') (ram comp')
       in Right (comp' {ram = ram'}, "")
    ADD -> do
      n <- getVal operand comp' lineNo
      let a = acc comp' + n
      Right (comp' {acc = a}, "")
    SUBTRACT -> do
      n <- getVal operand comp' lineNo
      let a = acc comp' - n
      Right (comp' {acc = a}, "")
    MULTIPLY -> do
      n <- getVal operand comp' lineNo
      let a = acc comp' * n
      Right (comp' {acc = a}, "")
    DIVIDE -> do
      n <- getVal operand comp' lineNo
      if n == 0
        then Left $ "Divide by zero error at line " ++ show lineNo ++ "\n"
        else let a = acc comp' `div` n
              in Right (comp' {acc = a}, "")
    JUMP -> do
      let AddrOperand a = operand
      Right (comp' {pc = a}, "")
    JIZERO -> do
      let AddrOperand a = operand
          comp'' =
            if acc comp' == 0
              then comp' {pc = a}
              else comp'
      Right (comp'', "")
    JINEG -> do
      let AddrOperand a = operand
          comp'' =
            if acc comp' < 0
              then comp' {pc = a}
              else comp'
      Right (comp'', "")
    NoOp -> Right (comp' {steps = steps comp}, "")

-- Get the value of a numeric operand, which may be a literal constant
-- or a reference to a stored variable.
getVal :: Operand -> Computer -> LineNo -> Either String Integer
getVal (ValueOperand (Left n)) _ _ = Right n
getVal (ValueOperand (Right s)) comp lineNo =
  case Map.lookup s $ ram comp of
    Just n -> Right n
    Nothing ->
      Left $ "Unknown variable: '" ++ s ++ "' at line " ++ show lineNo ++ "\n"
