module Scanner
  ( scanTokens
  , ScannerError(..)
  ) where

import Data.List (find, isPrefixOf)
import ScannedToken (ScannedToken)
import qualified ScannedToken as ST
import qualified Token as T

-- | Message and line number.
data ScannerError = ScannerError String Int deriving (Show)

type Source = String
type Location = (Int, Int) -- (Line, Col)

scanTokens :: Source -> ([ScannerError], [ScannedToken])
scanTokens wholeSource = scanTokens' wholeSource (0, 0)

-- | Input is a piece of lox source and position of its first character relative to the complete source.
scanTokens' :: Source -> Location -> ([ScannerError], [ScannedToken])
scanTokens' source location =
  let (scannerErrorOrToken, source', location') = scanToken source location
  in case scannerErrorOrToken of
       Left scannerError -> addError scannerError $ scanTokens' source' location'
       Right scannedToken -> if (ST._token scannedToken == T.Eof)
                             then ([], [])
                             else addToken scannedToken $ scanTokens' source' location'
  where
    addError e (es, ts) = (e : es, ts)
    addToken t (es, ts) = (es, t : ts)

-- | Example: If total source code is "foo = 5\nbar = 4", then input for SingleTokenScanner
-- might be "bar = 4" and (1, 0), and if it scans for identifier, output would be
-- (Right _, " = 4", (1, 3)).
type SingleTokenScanner = Source  -- * Piece of the source code to scan for the token.
                       -- | Absolute location of the first character of the given piece of the source code
                       -- in the whole source code.
                       -> Location
                       -- | Scanned token (or error), remainder of given piece of the source after scanning,
                       -- and location of the first character of that remainder in the whole source code.
                       -> (Either ScannerError ScannedToken, Source, Location)

scanToken :: SingleTokenScanner
scanToken "" location = (Right (ST.ScannedToken T.Eof (fst location)), "", location)
scanToken source@(c : source') location@(line, col)
  | isLineComment = skipLineComment
  | isWhiteSpace = skipWhiteSpace
  | isNewLine = skipNewLine
  | isStringLiteral = scanStringLiteral
  | isNumberLiteral = scanNumberLiteral
  | isIdentifierOrKeyword = scanIdentifierOrKeyword
  -- If simple lexeme, scan it.
  | Just (lexeme, token) <- find ((`isPrefixOf` source) . fst) simpleLexemeToToken =
      (Right (ST.ScannedToken token line), drop (length lexeme) source, (line, col + (length lexeme)))
  | otherwise = unexpectedCharacterError
  where
    isLineComment = "//" `isPrefixOf` source
    skipLineComment = let sourceWithoutComment = drop 1 $ dropWhile (/= '\n') source
                      in scanToken sourceWithoutComment (line + 1, 0)

    isWhiteSpace = c `elem` [' ', '\r', '\t']
    skipWhiteSpace = scanToken source' (line, col + 1)

    isNewLine = c == '\n'
    skipNewLine = scanToken source' (line + 1, 0)

    isStringLiteral = c == '"'
    scanStringLiteral = scanStringLiteral' "" source' (line, col + 1)

    isNumberLiteral = isDigit c
    scanNumberLiteral = scanNumberLiteral' source location

    isIdentifierOrKeyword = isAlpha c
    scanIdentifierOrKeyword = scanIdentifierOrKeyword' source location

    unexpectedCharacterError =
      (Left (ScannerError ("Unexpected character '" ++ [c] ++ "'.") line), (tail source), (line, col + 1))

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

isAlpha :: Char -> Bool
isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_')

isAlphaNumeric :: Char -> Bool
isAlphaNumeric c = isAlpha c || isDigit c

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

scanStringLiteral'
    :: String  -- * String prefix that was already scanned so far, but without initial " and reversed.
    -> SingleTokenScanner  -- * Scanner that will scan rest of the string and return token of the whole string.
scanStringLiteral' _ "" location =
  (Left (ScannerError "Unterminated string, expected \"." (fst location)), "", location)
scanStringLiteral' scannedReversed ('\"' : source') (line, col) =
  let value = reverse scannedReversed
      lexeme = '"' : value ++ "\""
  in (Right (ST.ScannedToken (T.String lexeme value) line), source', (line, col + 1))
scanStringLiteral' scannedReversed (c : source') (line, col) =
  let location' = if c == '\n' then (line + 1, col) else (line, col + 1)
  in scanStringLiteral' (c : scannedReversed) source' location'

scanNumberLiteral' :: SingleTokenScanner
scanNumberLiteral' source (line, col) =
  let (wholeNumberPart, source') = (takeWhile isDigit source, dropWhile isDigit source)
      (decimalNumberPart, source'') = if (length source' >= 2 && head source' == '.' && isDigit (source' !! 1))
                                      then ('.' : (takeWhile isDigit (drop 1 source')), dropWhile isDigit (drop 1 source'))
                                      else ("", source')
      lexeme = wholeNumberPart ++ decimalNumberPart
  in if (null lexeme)
     then (Left (ScannerError "Failed to scan number literal." line), source, (line, col))
     else (Right (ST.ScannedToken (T.Number lexeme (read lexeme)) line), source'', (line, col + length lexeme))

-- Scans identifier and returns either Identifier token or, if it is a keyword, appropriate keyword token.
scanIdentifierOrKeyword' :: SingleTokenScanner
scanIdentifierOrKeyword' source (line, col) =
  let (lexeme, source') = (takeWhile isAlphaNumeric source, dropWhile isAlphaNumeric source)
      token = case lookup lexeme keywordLexemeToToken of
                Just keywordToken -> keywordToken
                Nothing -> T.Identifier lexeme
  in if (not $ isAlpha $ head source)
     then (Left (ScannerError "Failed to scan identifier." line), source, (line, col))
     else (Right (ST.ScannedToken token line), source', (line, col + length lexeme))
  where
    keywordLexemeToToken = [ ("and", T.And)
                           , ("class", T.Class)
                           , ("else", T.Else)
                           , ("false", T.False)
                           , ("for", T.For)
                           , ("fun", T.Fun)
                           , ("if", T.If)
                           , ("nil", T.Nil)
                           , ("or", T.Or)
                           , ("print", T.Print)
                           , ("return", T.Return)
                           , ("super", T.Super)
                           , ("this", T.This)
                           , ("true", T.True)
                           , ("var", T.Var)
                           , ("while", T.While)
                           ]
