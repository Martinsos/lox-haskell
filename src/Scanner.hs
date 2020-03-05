module Scanner
  ( scanTokens
  , ParseError(..)
  ) where

import Data.List (find, isPrefixOf)
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
scanToken source (line, col)
  -- Skip line comment.
  | "//" `isPrefixOf` source =
      let source' = drop 1 $ dropWhile (/= '\n') source
      in scanToken source' (line + 1, 0)
  -- Skip white spaces.
  | (head source) `elem` [' ', '\r', '\t'] = scanToken (tail source) (line, col + 1)
  -- Skip new line.
  | head source == '\n' = scanToken (tail source) (line + 1, 0)
  -- Try to parse simple lexeme.
  | Just (lexeme, token) <- find ((`isPrefixOf` source) . fst) simpleLexemeToToken =
      (Right (PT.ParsedToken token line), drop (length lexeme) source, (line, col + (length lexeme)))
  -- Try to parse string.
  | head source == '"' = scanStringLiteral "" (tail source) (line, col + 1)
  -- Error
  | otherwise = (Left (ParseError ("Unexpected character '" ++ [head source] ++ "'.") line), (tail source), (line, col + 1))
  where
    -- NOTE: Order is important, longer ones should be first!
    simpleLexemeToToken = [ ("!=", T.BangEqual)
                          , ("==", T.EqualEqual)
                          , ("<=", T.LessEqual)
                          , (">=", T.GreaterEqual)
                          , ("!", T.Bang)
                          , ("=", T.Equal)
                          , ("<", T.Less)
                          , (">", T.Greater)
                          , ("(", T.LeftParen)
                          , (")", T.RightParen)
                          , ("{", T.LeftBrace)
                          , ("}", T.RightBrace)
                          , (",", T.Comma)
                          , (".", T.Dot)
                          , ("-", T.Minus)
                          , ("+", T.Plus)
                          , (";", T.Semicolon)
                          , ("*", T.Star)
                          , ("/", T.Slash)
                          ]

-- | Scans string literal, while assuming that initial " was already scanned.
scanStringLiteral :: String -> Source -> Location -> (Either ParseError ParsedToken, Source, Location)
scanStringLiteral _ "" location =
  (Left (ParseError "Unterminated string, expected \"." (fst location)), "", location)
scanStringLiteral scannedReversed ('\"' : source') (line, col) =
  let value = reverse scannedReversed
      lexeme = '"' : value ++ "\""
  in (Right (PT.ParsedToken (T.String lexeme value) line), source', (line, col + 1))
scanStringLiteral scannedReversed (c : source') (line, col) =
  let location' = if c == '\n' then (line + 1, col) else (line, col + 1)
  in scanStringLiteral (c : scannedReversed) source' location'

