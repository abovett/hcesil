-- Functions for executing the compiled CESIL code

module Execute
  ( runProgram
  ) where


import Control.Monad.IO.Class
import Control.Monad.Trans.State
import qualified Data.Map as Map

import Compile ( Instruction (..)
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
  evalStateT execute comp


-- Main execution "loop"
execute :: StateT Computer IO ()
execute = do
  output <- step
  liftIO $ putStr output
  comp <- get
  if halted comp
    then return ()
    else execute


-- Execute a single step/cycle
step :: Monad m => StateT Computer m String
step = do
  comp <- get
  -- TODO clean up. Can we use another Monad transformer?
  if pc comp >= (toInteger . length . program $ comp)
    then do put $ comp{ halted = True }
            return $ "PC OUT OF RANGE: " ++ (show $ pc comp) ++ "\n"
    else do let pcv = fromIntegral $ pc comp
                Instruction lineNo opCode operand = program comp !! pcv
            case opCode of
              HALT -> do
                put $ comp{ halted = True }
                return "HALT\n"
              LINE -> do
                let comp' = comp{ pc = pc comp + 1 }
                put comp'
                return "\n"
              PRINT -> do
                let TextOperand s = operand
                    comp' = comp{ pc = pc comp + 1 }
                put comp'
                return s
              STORE -> do
                let SymbolOperand s = operand
                    comp' = comp{ pc = pc comp + 1
                                , ram = Map.insert s (acc comp) (ram comp) }
                put comp'
                return "\n"
              _ -> do
                let comp' = comp{ pc = pc comp + 1 }
                put comp'
                return $ "step: PC = " ++  (show $ pc comp') ++ "\n"
