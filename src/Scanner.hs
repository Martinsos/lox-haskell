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
-- TODO: Can I refactor this method so that I extract almost all parsing logic for specific type of token into functions, so I don't need any comments in it?
scanToken :: Source -> Location -> (Either ParseError ParsedToken, Source, Location)
scanToken "" location = (Right (PT.ParsedToken T.Eof (fst location)), "", location)
scanToken source@(c : _) (line, col)
  -- Skip line comment.
  | "//" `isPrefixOf` source =
      let source' = drop 1 $ dropWhile (/= '\n') source
      in scanToken source' (line + 1, 0)
  -- Skip white space.
  | c `elem` [' ', '\r', '\t'] = scanToken (tail source) (line, col + 1)
  -- Skip new line.
  | c == '\n' = scanToken (tail source) (line + 1, 0)
  -- Try to parse string literal.
  | c == '"' = scanStringLiteral "" (tail source) (line, col + 1)
  -- Trye to parse number literal.
  | isDigit c = scanNumberLiteral source (line, col)
  -- Try to parse simple lexeme.
  | Just (lexeme, token) <- find ((`isPrefixOf` source) . fst) simpleLexemeToToken =
      (Right (PT.ParsedToken token line), drop (length lexeme) source, (line, col + (length lexeme)))
  -- Error
  | otherwise = (Left (ParseError ("Unexpected character '" ++ [c] ++ "'.") line), (tail source), (line, col + 1))
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

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

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

scanNumberLiteral :: Source -> Location -> (Either ParseError ParsedToken, Source, Location)
scanNumberLiteral source (line, col) =
  let (wholeNumberPart, source') = (takeWhile isDigit source, dropWhile isDigit source)
      (decimalNumberPart, source'') = if (length source' >= 2 && head source' == '.' && isDigit (source' !! 1))
                                      then ('.' : (takeWhile isDigit (drop 1 source')), dropWhile isDigit (drop 1 source'))
                                      else ("", source')
      lexeme = wholeNumberPart ++ decimalNumberPart
  in if (null lexeme)
     then (Left (ParseError "Failed to scan number literal." line), source, (line, col))
     else (Right (PT.ParsedToken (T.Number lexeme (read lexeme)) line), source'', (line, col + length lexeme))
