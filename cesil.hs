-- CESIL interpreter top level

import System.Environment
import Control.Exception
import Control.Monad.Except
import qualified Data.Map as Map

import ScanSource

main :: IO ()
main = handleErrors <=< runExceptT $ do
  args <- liftIO getArgs
  filename <- parseArgs args
  source <- readSource filename
  res <- compileAndRun source
  liftIO $ printReport filename res


-- Report an error if present.
handleErrors :: Either String () -> IO ()
handleErrors (Left err) = putStrLn $ "ERROR: " ++ err
handleErrors (Right _) = return ()


-- Parse the command line and return the filename or an error
parseArgs :: [String] -> ExceptT String IO  String
parseArgs args
  | length args == 0 = throwError "No filename specified."
  | length (head args) == 0 = throwError "Empty filename."
  | length args > 1 = throwError "Too many parameters"
  | otherwise = return $ head args


-- Open and read in the source file.
readSource :: String -> ExceptT String IO String
readSource filename = ExceptT $ do
  putStrLn $ "Loading: " ++ filename
  res <- try $ readFile filename :: IO (Either SomeException String)
  case res of
    Left err -> return . Left . show $ err
    Right contents -> return $ Right contents


-- "Compile" and run the CESIL program.
compileAndRun :: String -> ExceptT String IO String
compileAndRun source =
  let ss = scanSource source
  in return $ show ss


-- TODO dummy code
printReport :: String -> String -> IO ()
printReport filename res = do
  putStrLn $ "Report for " ++ filename
  putStrLn res
