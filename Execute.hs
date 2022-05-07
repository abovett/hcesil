-- Functions for executing the compiled CESIL code

module Execute
  ( runProgram
  ) where


import Control.Monad.Except
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
execute :: Computer -> IO ()
execute comp = handleErrors <=< runExceptT $ do
  liftEither $ checkPC comp
  (comp', output) <- liftEither . step $ comp
  liftIO $ putStr output
  if halted comp'
    then return ()
    else liftIO . execute $ comp'


-- Format and report an error.
handleErrors :: Either String () -> IO ()
handleErrors (Left err) = putStrLn $ "*** ERROR: " ++ err
handleErrors (Right _) = return ()


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
  let Instruction lineNo opCode operand = program comp !! (fromIntegral . pc $ comp)
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

    -- TODO eliminate this once all instructions implemented.
    _ ->
      let comp' = comp{ pc = pc comp + 1 }
          output = "step: PC = " ++  (show $ pc comp') ++ "\n"
      in Right ( comp', output )


-- Get the value of a numeric operand, which may be a literal constant
-- or a reference to a stored variable.
getVal :: Operand -> Computer -> LineNo -> Either String Integer
getVal (ValueOperand (Left n)) _ _ = Right n
getVal (ValueOperand (Right s)) comp lineNo =
  case Map.lookup s $ ram comp of
    Just n -> Right n
    Nothing -> Left $ "Unknown variable: '"
      ++ s ++ "' at line " ++ show lineNo ++ "\n"
