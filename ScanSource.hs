-- Scan a CESIL source file and return its parts in a structured form.

module ScanSource
  ( scanSource
  , ScannedSource (ScannedSource)
  , CodeLine (CodeLine)
  , DataLine (DataLine)
  ) where

import Data.Char

-- Type to contain the sorce code after scanning/lexing
data ScannedSource = ScannedSource [CodeLine] Bool [DataLine] deriving (Show)

-- A scanned line of source comprising line number, label, instruction
-- and operand.
data CodeLine = CodeLine Integer String String String deriving (Show)

-- A scanned data line with line number
data DataLine = DataLine Integer String deriving (Show)


-- Scan the source lines and return a ScannedSource item
scanSource :: String -> ScannedSource
scanSource source = let
  numberedLines = zip [1..] $ lines source
  start = ScannedSource [] False []
  ScannedSource cl pcount dl = foldl ssBuilder start numberedLines
  in ScannedSource (reverse cl) pcount (reverse dl)

-- "Accumulator" to build the ScannedSource item
ssBuilder :: ScannedSource -> (Integer, String) -> ScannedSource
ssBuilder (ScannedSource cl pcent dl) (lineNo, sLine)
  | isCommentOrBlank sLine = ScannedSource cl pcent dl
  | not pcent = if isDataStart sLine
                then ScannedSource cl True dl
                else ScannedSource (scanSourceLine lineNo sLine : cl) pcent dl
  | otherwise = ScannedSource cl pcent (scanDataLine lineNo sLine : dl)

-- Is this line a comment or blank?
isCommentOrBlank :: String -> Bool
isCommentOrBlank sLine
  | dropWhile isSpace sLine == "" = True
  | head sLine == '*' = True
  | otherwise = False

-- Is this the start of the data i.e. does this line contain a single %?
isDataStart :: String -> Bool
isDataStart sLine = let
  s = dropWhile isSpace sLine
  in length s > 0 && head s == '%' && (dropWhile isSpace $ tail s) == ""

-- Generate a CodeLine "object" from a source line, by splitting a
-- source line into label, instruction and operand. The line number is
-- also passed in and is included in the returned CodeLine.
scanSourceLine :: Integer -> String -> CodeLine
scanSourceLine lineNo ln = let
  (label, rest) = break isSpace ln
  rest' = dropWhile isSpace rest
  (instr, rest'') = break isSpace rest'
  param = reverse $ dropWhile isSpace rest''
  param' = reverse $ dropWhile isSpace param
  in CodeLine lineNo label instr param'

-- Generate a DataLine "object" from a data line by removing spaces.
-- The line number is also passed in and is included in the returned
-- DataLine.
scanDataLine :: Integer -> String -> DataLine
scanDataLine lineNo ln = let
  ln' = reverse $ dropWhile isSpace ln
  ln'' = reverse $ dropWhile isSpace ln'
  in DataLine lineNo ln''
