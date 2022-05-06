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
  execute comp


-- Main execution "loop"
execute :: Computer -> IO ()
execute comp = do
  let (comp', output) = step comp
  putStr output
  if halted comp'
    then return ()
    else execute comp'


-- Execute a single step/cycle
step :: Computer -> (Computer, String)
step comp
  | pc comp >= (toInteger . length . program $ comp) =
    ( comp{ halted = True }
    , "PC OUT OF RANGE: " ++ (show $ pc comp) ++ "\n"
    )

  | otherwise =
      let pcv = fromIntegral $ pc comp
          Instruction lineNo opCode operand = program comp !! pcv
      in case opCode of
        HALT -> ( comp{ halted = True }, "HALT\n" )
        LINE -> ( comp{ pc = pc comp + 1 } , "\n" )
        PRINT -> let TextOperand s = operand
                 in ( comp{ pc = pc comp + 1 }, s )
        STORE -> let SymbolOperand s = operand
                     comp'=  comp{ pc = pc comp + 1
                                 , ram = Map.insert s (acc comp) (ram comp) }
                 in ( comp', "" )
        _ -> let comp' = comp{ pc = pc comp + 1 }
                 output = "step: PC = " ++  (show $ pc comp') ++ "\n"
             in ( comp', output )
