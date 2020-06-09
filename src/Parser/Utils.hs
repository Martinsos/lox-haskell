module Parser.Utils
    ( ifToken
    , consumeToken
    , consumeIdentifierToken
    , errorAtToken
    , errorAtEof
    ) where

import Data.Maybe (fromJust)
import qualified ScannedToken as ST
import qualified Token as T
import TokenParser (Parser, ParseError(..), Position(..), handleToken, popToken, logAndThrowError)


ifToken :: (T.Token -> Bool)
        -> (ST.ScannedToken -> Parser ST.ScannedToken a)
        -> Parser ST.ScannedToken a
        -> Parser ST.ScannedToken a
ifToken predicate onTrue onFalse = handleToken
    (\st -> if predicate (ST._token st) then onTrue st else onFalse)
    onFalse

-- | If next token satisfies given predicate, pop it and return it, otherwise report error with correct location.
consumeToken :: (T.Token -> Bool) -> String -> Parser ST.ScannedToken ST.ScannedToken
consumeToken predicate errorMsg = handleToken
    (\st -> if predicate (ST._token st)
            then popToken >>= return . fromJust
            else logAndThrowError (errorAtToken errorMsg st))
    (logAndThrowError (errorAtEof errorMsg))

consumeIdentifierToken :: Parser ST.ScannedToken (ST.ScannedToken, T.IdentifierLiteral)
consumeIdentifierToken = handleToken
    (\st -> case ST._token st of
                T.Identifier literal -> popToken >> return (st, literal)
                _ -> logAndThrowError (errorAtToken errorMsg st))
    (logAndThrowError (errorAtEof errorMsg))
  where errorMsg = "Expected identifier."

errorAtToken :: String -> ST.ScannedToken -> ParseError
errorAtToken msg st = ParseError msg (LineNumber $ ST._line st)

errorAtEof :: String -> ParseError
errorAtEof msg = ParseError msg Eof
