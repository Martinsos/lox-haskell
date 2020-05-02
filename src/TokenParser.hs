module TokenParser
    ( Parser
    , ParseError(..)
    , Position(..)
    , runParser
    , peekToken
    , popToken
    , logError
    , logAndThrowError
    ) where

import Utils (safeHead)
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Control.Monad.State.Lazy (State, runState, gets, modify)


type Parser t a = ExceptT ParseError (State (ParserState t)) a

data ParserState t = ParserState
    { _tokens :: [t]
    , _errors :: [ParseError]
    }

data ParseError = ParseError String Position deriving (Show)

data Position = LineNumber Int | Eof deriving (Show)


peekToken :: Parser t (Maybe t)
peekToken = getTokens >>= return . safeHead

popToken :: Parser t (Maybe t)
popToken = do
    tokens <- getTokens
    setTokens (tail tokens)
    return $ safeHead tokens

getTokens :: Parser t [t]
getTokens = gets _tokens

setTokens :: [t] -> Parser t ()
setTokens ts = modify $ \parserState -> parserState { _tokens = ts }

logError :: ParseError -> Parser t ()
logError e = modify $ \parserState@(ParserState { _errors = errors }) -> parserState { _errors = e:errors }

logAndThrowError :: ParseError -> Parser t a
logAndThrowError e = logError e >> throwError e


-- | Runs given parser on provided tokens and returns parsed expression, errors it recovered from and remaining tokens.
-- If there was an error parser could not recover from, Nothing is returned in place of parsed expression.
runParser :: Parser t a -> [t] -> (Maybe a, [ParseError], [t])
runParser parser tokens =
    let (resultOrError, parserState) = runState (runExceptT parser) initialParserState
        maybeResult = either (const Nothing) (Just . id) resultOrError
    in (maybeResult, _errors parserState, _tokens parserState)
  where initialParserState = ParserState { _tokens = tokens, _errors = [] }


