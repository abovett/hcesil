-- CESIL interpreter top level
-- TODO implement maxsteps
import Control.Exception
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.List
import System.Environment

import Compile
import Execute
import ParseArgs

progVersion = "hcesil version 1.1.0"

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
          liftIO $ runProgram program dataVals

-- Report an error if present.
handleErrors :: Either String () -> IO ()
handleErrors (Left err) = putStrLn $ "*** ERROR: " ++ err
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
