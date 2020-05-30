module Parser.Utils
    ( ifToken
    , consumeToken
    , consumeIdentifierToken
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
            else logAndThrowError (ParseError errorMsg (LineNumber $ ST._line st)))
    (logAndThrowError (ParseError errorMsg Eof))

consumeIdentifierToken :: Parser ST.ScannedToken (ST.ScannedToken, T.IdentifierLiteral)
consumeIdentifierToken = handleToken
    (\st -> case ST._token st of
                T.Identifier literal -> popToken >> return (st, literal)
                _ -> logAndThrowError (ParseError errorMsg (LineNumber $ ST._line st)))
    (logAndThrowError (ParseError errorMsg Eof))
  where
    errorMsg = "Expected identifier."
