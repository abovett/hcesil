-- CESIL interpreter top level

-- TODO check all these are needed
import System.Environment
import System.IO
import System.IO.Error
import Control.Exception

import System.Exit

main :: IO ()
main = do
  args <- getArgs
  filename <- case parseArgs args of
        Left err -> die (err)
        Right f -> return f

  contents <- readSource filename

  let l = length contents
  putStrLn $ "Done: " ++ show l

readSource :: String -> IO String
readSource filename = do
  putStrLn $ "Process " ++ filename
  contents <- readFile filename `catch` handler
  return contents

handler :: IOError -> IO String
handler e
  | isDoesNotExistError e = dieErr e "File not found"
  | isAlreadyInUseError e = dieErr e "File in use"
  | isEOFError e = dieErr e "Unexpected end of file"
  | isPermissionError e = dieErr e "Permission error"
  | otherwise = ioError e
  where dieErr e m =
          case ioeGetFileName e of Just path -> die $ m ++ ": " ++ path
                                   Nothing -> die $ m ++ " - filename not known."

-- Parse the command line and return the filename or an error
-- TODO improve this
parseArgs :: [String] -> Either String String
parseArgs args
  | length args == 0 = Left "ERROR: No filename specified."
  | length (head args) == 0 = Left "ERROR: Empty filename."
  | length args > 1 = Left "ERROR: Too many parameters"
  | otherwise = Right $ head args
