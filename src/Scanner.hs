module Scanner
  ( scanTokens
  , ParseError(..)
  ) where

import ParsedToken (ParsedToken)
import qualified ParsedToken as PT
import qualified Token as T

-- | Message and line number.
data ParseError = ParseError String Int deriving (Show)

type Source = String
type Location = (Int, Int) -- (Line, Col)

scanTokens :: Source -> ([ParseError], [ParsedToken])
scanTokens wholeSource = scanTokens' wholeSource (0, 0)

-- | Input is a piece of lox source and position of its first character relative to the complete source.
scanTokens' :: Source -> Location -> ([ParseError], [ParsedToken])
scanTokens' source location =
  let (parseErrorOrToken, source', location') = scanToken source location
  in case parseErrorOrToken of
       Left parseError -> addError parseError $ scanTokens' source' location'
       Right ptoken -> if (PT._token ptoken == T.Eof)
                       then ([], [])
                       else addToken ptoken $ scanTokens' source' location'
  where
    addError e (es, ts) = (e : es, ts)
    addToken t (es, ts) = (es, t : ts)

-- | Input is source and location of first character in source.
-- Output is parsed token or error, remaining source after parsing and location of first character in the remaining source.
scanToken :: Source -> Location -> (Either ParseError ParsedToken, Source, Location)
scanToken "" location = (Right (PT.ParsedToken T.Eof (fst location)), "", location)
scanToken (c : source') (line, col)
  | Just token <- lookup c singleCharLexemeToToken = (Right (PT.ParsedToken token line), source', (line, col + 1))
  | otherwise = (Left (ParseError ("Unexpected character '" ++ [c] ++ "'.") line), source', (line, col + 1))
  where
    singleCharLexemeToToken = [ ('(', T.LeftParen)
                              , (')', T.RightParen)
                              , ('{', T.LeftBrace)
                              , ('}', T.RightBrace)
                              , (',', T.Comma)
                              , ('.', T.Dot)
                              , ('-', T.Minus)
                              , ('+', T.Plus)
                              , (';', T.Semicolon)
                              , ('*', T.Star)
                              ]
