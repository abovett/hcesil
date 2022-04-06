-- CESIL interpreter top level

import System.Environment
import Control.Exception
import Control.Monad.Except
import Data.List

import Compile


main :: IO ()
main = handleErrors <=< runExceptT $ do
  args <- liftIO getArgs
  filename <- parseArgs args
  source <- readSource filename
  let program = compile source

  -- TODO WIP
  liftIO $ printReport filename program


-- Report an error if present.
handleErrors :: Either String () -> IO ()
handleErrors (Left err) = putStrLn $ "ERROR: " ++ err
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
  putStrLn $ "Loading: " ++ filename
  res <- try $ readFile filename :: IO (Either SomeException String)
  case res of
    Left err -> return . Left . show $ err
    Right contents -> return $ Right contents


-- TODO dummy code
printReport :: String -> Either [String] ([Instruction], [Integer]) -> IO ()
printReport filename program = do
  let res = case program of
        Left errors -> concat $ intersperse "\n" errors
        Right (code, dataVals) -> let fp = replace ',' '\n' $ show code
                                      fd = replace ',' '\n' $ show dataVals
                                  in fp ++ "\n\n" ++ fd
  putStrLn res


-- TODO debug helper code
replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map $ \c -> if c == a then b else c
