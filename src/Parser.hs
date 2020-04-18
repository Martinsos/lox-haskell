module Parser
    ( expression
    , runParser
    ) where

import Utils (safeHead)
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Control.Monad.State.Lazy (State, runState, gets, modify)
import qualified ScannedToken as ST
import qualified Token as T
import qualified AST


type Parser a = ExceptT ParseError (State ParserState) a
data ParserState = ParserState
    { _tokens :: [ST.ScannedToken]
    , _errors :: [ParseError]
    }
data ParseError = ParseError String Position deriving (Show)
data Position = LineNumber Int | Eof deriving (Show)

peekToken :: Parser (Maybe ST.ScannedToken)
peekToken = getTokens >>= return . safeHead

popToken :: Parser (Maybe ST.ScannedToken)
popToken = do
    tokens <- getTokens
    setTokens (tail tokens)
    return $ safeHead tokens

getTokens :: Parser [ST.ScannedToken]
getTokens = gets _tokens

setTokens :: [ST.ScannedToken] -> Parser ()
setTokens ts = modify $ \parserState -> parserState { _tokens = ts }

logError :: ParseError -> Parser ()
logError e = modify $ \parserState@(ParserState { _errors = errors }) -> parserState { _errors = e:errors }

logAndThrowError :: ParseError -> Parser a
logAndThrowError e = logError e >> throwError e


-- | Runs given parser on provided tokens and returns parsed expression and remaining tokens.
runParser :: Parser a -> [ST.ScannedToken] -> (Maybe a, [ParseError], [ST.ScannedToken])
runParser parser scannedTokens =
    let (resultOrError, parserState) = runState (runExceptT parser) initialParserState
        maybeResult = either (const Nothing) (Just . id) resultOrError
    in (maybeResult, _errors parserState, _tokens parserState)
  where initialParserState = ParserState { _tokens = scannedTokens, _errors = [] }


expression :: Parser AST.Expr
expression = equality

-- | equality -> comparison ( ("!=" | "==" ) comparison )*
equality :: Parser AST.Expr
equality = makeBinaryOperationParser [(T.EqualEqual, AST.Equal), (T.BangEqual, AST.NotEqual)] comparison

-- | comparison -> addition ( (">" | ">=" | "<" | "<=" ) addition )*
comparison :: Parser AST.Expr
comparison = makeBinaryOperationParser operators addition
  where operators = [ (T.Less, AST.Less)
                    , (T.LessEqual, AST.LessEqual)
                    , (T.Greater, AST.Greater)
                    , (T.GreaterEqual, AST.GreaterEqual)
                    ]

-- | addition -> multiplication ( ("-" | "+" ) multiplication )*
addition :: Parser AST.Expr
addition = makeBinaryOperationParser [(T.Minus, AST.Minus), (T.Plus, AST.Plus)] multiplication

-- | multiplication -> unary ( ("*" | "/" ) unary )*
multiplication :: Parser AST.Expr
multiplication = makeBinaryOperationParser [(T.Star, AST.Star), (T.Slash, AST.Slash)] unary

-- | unary -> ( "!" | "-" ) unary | primary
unary :: Parser AST.Expr
unary = do
    maybeToken <- peekToken
    case maybeToken of
        Just t | ST._token t == T.Bang  -> popToken >> parseUnary AST.Not
               | ST._token t == T.Minus -> popToken >> parseUnary AST.UnaryMinus
        _ -> primary
  where parseUnary operator = do
            subExpr <- unary
            return $ AST.UnaryOperatorExpr operator subExpr

-- | NUMBER | STRING | "false" | "true" | "nil" | "(" expression ")"
primary :: Parser AST.Expr
primary = do
    maybeToken <- peekToken
    case maybeToken of
        Just t -> case ST._token t of
            T.False            -> popToken >> return (AST.LiteralExpr $ AST.BooleanLiteral False)
            T.True             -> popToken >> return (AST.LiteralExpr $ AST.BooleanLiteral True)
            T.Nil              -> popToken >> return (AST.LiteralExpr AST.NilLiteral)
            (T.Number _ value) -> popToken >> return (AST.LiteralExpr $ AST.NumberLiteral value)
            (T.String _ value) -> popToken >> return (AST.LiteralExpr $ AST.StringLiteral value)
            T.LeftParen        -> popToken >> do
                subExpr <- expression
                maybeRightParen <- peekToken
                case maybeRightParen of
                    Just rp | ST._token rp == T.RightParen -> popToken >> return subExpr
                    _ -> logAndThrowError (ParseError "Expected ')'." Eof)
            _ -> logAndThrowError (ParseError "Expected primary expression." (LineNumber $ ST._line t))
        Nothing -> logAndThrowError (ParseError "Expected primary expression." Eof)

-- | Given a sub expression and a list of operators, creates parser for
-- following grammar production: <subExpr> ( ( <op1> | ... | <opN> ) <subExpr> )*
makeBinaryOperationParser
    :: [(T.Token, AST.BinaryOperator)] -- * Valid binary operators tokens and their corresponding AST operators.
    -> Parser AST.Expr -- * Sub-expression parser.
    -> Parser AST.Expr -- * Either a binary operator expression or, if there is none, just sub-expression.
makeBinaryOperationParser operators subExprParser = do
    lhe <- subExprParser
    tryParsingRhe lhe
  where
    -- | Given lhe expression, returns parser that tries to parse the following binary operator and rhe expression.
    -- If there is no binary operator following, it just returns the given lhe expression.
    tryParsingRhe :: AST.Expr -> Parser AST.Expr
    tryParsingRhe lhe = do
        maybeToken <- peekToken
        case maybeToken >>= \t -> lookup (ST._token t) operators of
            Just operator -> do
                _ <- popToken
                rhe <- subExprParser
                let lhe' = AST.BinaryOperatorExpr operator lhe rhe
                tryParsingRhe lhe'
            Nothing -> return lhe
