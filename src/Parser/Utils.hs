module Parser.Utils
    ( expectToken
    , consumeToken
    ) where

import qualified ScannedToken as ST
import qualified Token as T
import TokenParser (Parser, ParseError(..), Position(..), handleToken, popToken, logAndThrowError)


expectToken
    :: T.Token
    -> (ST.ScannedToken -> Parser ST.ScannedToken a)
    -> (ST.ScannedToken -> Parser ST.ScannedToken a)
    -> Parser ST.ScannedToken a
    -> Parser ST.ScannedToken a
expectToken expectedToken onMatch onMismatch onEnd = handleToken
    (\st -> if ST._token st == expectedToken
            then onMatch st
            else onMismatch st)
    onEnd

-- | If next token matches specified token, parses/pops it and returns it.
-- Otherwise, it throws given error message with correct location reported.
consumeToken :: T.Token -> String -> Parser ST.ScannedToken (Maybe ST.ScannedToken)
consumeToken token errorMsg = expectToken token
    (\_ -> popToken)
    (\st -> logAndThrowError (ParseError errorMsg (LineNumber $ ST._line st)))
    (logAndThrowError (ParseError errorMsg Eof))
