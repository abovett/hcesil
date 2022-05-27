-- hcesil - a CESIL interpreter
-- (C) Andrew Bovett 2022
-- License: GPLv3
import Control.Exception
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.List
import System.Environment

import Compile
import Execute
import ParseArgs

progVersion =
  "hcesil version 1.4.0\n\
  \Copyright (C) 2022 Andrew Bovett.\n\
  \License GPLv3: GNU GPL version 3 <https://gnu.org/licenses/gpl.html>.\n\
  \This is free software: you are free to change and redistribute it\n\
  \under the terms of the above license.\n\
  \There is NO WARRANTY, to the extent permitted by law."

-- Top level of program
main :: IO ()
main =
  handleErrors <=< runExceptT $ do
    args <- liftIO getArgs
    pars <- liftEither $ parseArgs args
    case () of
      _
        | help pars -> liftIO $ putStrLn helpText
        | version pars -> liftIO $ putStrLn progVersion
        | otherwise -> do
          fname <-
            case filename pars of
              Nothing -> throwError "No filename specified."
              Just fname -> return fname
          source <- readSource fname
          (program, dataVals) <- liftEither $ compile source
          runProgram program dataVals pars

-- Report an error if present.
handleErrors :: Either String () -> IO ()
handleErrors (Left err) = putStrLn $ "\n*** ERROR: " ++ err
handleErrors (Right _) = return ()

-- Open and read in the source file.
readSource :: String -> ExceptT String IO String
readSource filename =
  ExceptT $ do
    res <- try $ readFile filename :: IO (Either SomeException String)
    return $
      case res of
        Left err -> Left . show $ err
        Right contents -> Right contents
