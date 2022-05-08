-- CESIL interpreter top level

import System.Environment
import Control.Exception
import Control.Monad.Except
import Data.List

import Compile
import Execute

main :: IO ()
main = handleErrors <=< runExceptT $ do
  args <- liftIO getArgs
  filename <- parseArgs args
  source <- readSource filename
  (program, dataVals) <- liftEither $ compile source
  liftIO $ runProgram program dataVals


-- Report an error if present.
handleErrors :: Either String () -> IO ()
handleErrors (Left err) = putStrLn $ "*** ERROR: " ++ err
handleErrors (Right _) = return ()


-- Parse the command line and return the filename or an error
parseArgs :: [String] -> ExceptT String IO String
parseArgs args
  | length args == 0 = throwError "No filename specified."
  | length (head args) == 0 = throwError "Empty filename."
  | length args > 1 = throwError "Too many parameters"
  | otherwise = return $ head args


-- Open and read in the source file.
readSource :: String -> ExceptT String IO String
readSource filename = ExceptT $ do
  res <- try $ readFile filename :: IO (Either SomeException String)
  return $ case res of
    Left err -> Left . show $ err
    Right contents -> Right contents
