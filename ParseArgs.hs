-- Command line parser
module ParseArgs
  ( Params(..)
  , parseArgs
  , helpText
  ) where

import Text.Read

import Common (Params(..))

-- Parse the command line
parseArgs :: [String] -> Either String Params
parseArgs args =
  let pars =
        Params
          { filename = Nothing
          , help = False
          , version = False
          , countSteps = False
          , maxSteps = Nothing
          }
   in argParser args pars

-- Command line parsing "loop"
argParser :: [String] -> Params -> Either String Params
argParser [] pars = Right pars
argParser [[]] pars = Left "Empty parameter on command line."
argParser (a:ra) pars = do
  (args', pars') <-
    case () of
      _
        | a `elem` ["-h", "--help"] -> Right (ra, pars {help = True})
        | a `elem` ["-v", "--version"] -> Right (ra, pars {version = True})
        | a `elem` ["-c", "--countsteps"] ->
          Right (ra, pars {countSteps = True})
        | a `elem` ["-m", "--maxsteps"] ->
          (\n -> (tail ra, pars {maxSteps = Just n})) <$> (getPosInt a ra)
        | (head a) == '-' -> Left $ "Unrecognised option: " ++ a
        | null . filename $ pars -> Right (ra, pars {filename = Just a})
        | otherwise -> Left $ "Unexpected argument: " ++ a
  argParser args' pars'
  where
    getPosInt :: String -> [String] -> Either String Integer
    getPosInt a [] = Left $ "Missing parameter after option: " ++ a
    getPosInt a ra =
      case readMaybe $ head ra :: Maybe Integer of
        Just n ->
          if n > 0
            then Right n
            else Left $ "Option " ++ a ++ " parameter must be > 0"
        Nothing -> Left $ "Invalid parameter after option: " ++ a

-- Define help text
-- TODO write help text
helpText = "Help text not yet written"
