-- Functions for executing the compiled CESIL code

module Execute
  ( runProgram
  ) where


import Control.Monad.IO.Class
import Control.Monad.Trans.State
import qualified Data.Map as Map

import Common ( LineNo
              , Address
              , Instruction (..)
              , OpCode (..)
              , Operand (..)
              )

-- Define the computer state
data Computer = Computer { program :: [Instruction]
                         , dataVals :: [Integer]
                         , ram :: Map.Map String Integer
                         , acc :: Integer
                         , pc :: Integer
                         , halted :: Bool
                         }


-- Initialise the computer and run the program
runProgram pr dv = do
  let comp = Computer { program = pr
                      , dataVals = dv
                      , ram = Map.empty
                      , acc = 0
                      , pc = 0
                      , halted = False
                      }
  execute comp


-- Main execution "loop"
-- TODO can this use a monad?
execute :: Computer -> IO ()
execute comp = do
  case step comp of
    Left err -> putStrLn err
    Right (comp', output) -> do
      putStr output
      if halted comp'
        then return ()
        else execute comp'

-- TODO add line number to error references

-- Execute a single step/cycle
step :: Computer -> Either String (Computer, String)
step comp = do
  pcv <- if pc comp >= (toInteger . length . program $ comp)
         then Left $ "PC OUT OF RANGE: " ++ (show $ pc comp) ++ "\n"
         else Right . fromIntegral . pc $ comp

  let Instruction lineNo opCode operand = program comp !! pcv

  case opCode of
    HALT -> Right ( comp{ halted = True }, "\nHALT\n" )

    LINE -> Right ( comp{ pc = pc comp + 1 } , "\n" )

    PRINT ->
      let TextOperand s = operand
      in Right ( comp{ pc = pc comp + 1 }, s )

    LOAD -> do
      n <- getVal operand comp lineNo
      Right ( comp { pc = pc comp + 1, acc = n }, "")

    STORE ->
      let SymbolOperand s = operand
          ram' = Map.insert s (acc comp) (ram comp)
      in Right ( comp{ pc = pc comp + 1, ram = ram' }, "" )

    _ ->
      let comp' = comp{ pc = pc comp + 1 }
          output = "step: PC = " ++  (show $ pc comp') ++ "\n"
      in Right ( comp', output )


getVal :: Operand -> Computer -> LineNo -> Either String Integer
getVal (ValueOperand (Left n)) _ _ = Right n
getVal (ValueOperand (Right s)) comp lineNo =
  case Map.lookup s $ ram comp of
    Just n -> Right n
    Nothing -> Left $ "\nERROR: Unknown variable: '"
      ++ s ++ "' at line " ++ show lineNo ++ "\n"
