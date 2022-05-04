-- Functions for executing the compiled CESIL code

module Execute
  ( runProgram
  ) where


import Control.Monad.IO.Class
import Control.Monad.Trans.State

import Compile ( Instruction (..)
               , OpCode (..)
  , Operand (..)
               )

-- Define the computer state
-- TODO add RAM
data Computer = Computer { program :: [Instruction]
                         , dataVals :: [Integer]
                         , acc :: Int
                         , pc :: Int
                         , halted :: Bool
                         }


-- Initialise the computer and run the program
runProgram pr dv = do
  let comp = Computer { program = pr
                      , dataVals = dv
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
   -- TODO dummy stub code
   -- TODO handle out of range PC
   let Instruction lineNo opCode operand = program comp !! pc comp
   case opCode of
     HALT -> do
       put $ comp{ halted = True }
       return "HALT\n"
     LINE -> do
       let comp' = comp{ pc = pc comp + 1 }
       put comp'
       return "\n"
     PRINT -> do
       let comp' = comp{ pc = pc comp + 1 }
       put comp'
       let TextOperand s = operand
       return s
     _ -> do
       let comp' = comp{ pc = pc comp + 1 }
       put comp'
       return $ "step: PC = " ++  (show $ pc comp') ++ "\n"
