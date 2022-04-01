-- Scan CESIL source file and return its parts in a structured form.

module ScanSource
  ( scanSource
  , ScannedSource (ScannedSource)
  , CodeLine (CodeLine)
  ) where

import Data.Char

-- Type to contain the sorce code after scanning/lexing
data ScannedSource = ScannedSource [CodeLine] Bool [String] deriving (Show)

-- Scan the source lines and return a ScannedSource item
scanSource :: String -> ScannedSource
scanSource source = let
  sLines = lines source
  start = ScannedSource [] False []
  ScannedSource cl pcount dl = foldl ssBuilder start sLines
  in ScannedSource (reverse cl) pcount (reverse dl)

-- "Accumulator" to build the ScannedSource item
ssBuilder :: ScannedSource -> String -> ScannedSource
ssBuilder (ScannedSource cl pcent dl) sLine
  | isCommentOrBlank sLine = ScannedSource cl pcent dl
  | not pcent = if isDataStart sLine
                then ScannedSource cl True dl
                else ScannedSource (lineSplit sLine : cl) False dl
  | otherwise = ScannedSource cl True (dropWhile isSpace sLine : dl)

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

-- A line of source split into label, instruction and operand
data CodeLine = CodeLine String String String deriving (Show)

-- Split a source line into label, instruction and operand
lineSplit :: String -> CodeLine
lineSplit ln = let
  (label, rest) = break isSpace ln
  rest' = dropWhile isSpace rest
  (instr, rest'') = break isSpace rest'
  param = reverse $ dropWhile isSpace rest''
  param' = reverse $ dropWhile isSpace param
  in CodeLine label instr param'
