module Parser
    ( expression
    , runParser
    ) where

import Data.Maybe (listToMaybe)
import Control.Monad.State.Lazy (State, state, runState)
import qualified ScannedToken as ST
import qualified Token as T
import qualified AST


type ParserState = [ST.ScannedToken]
type Parser a = State ParserState a

-- TODO: Add error handling.
-- type Parser a = ExceptT ParserError (State ParserState) a
-- I will need to modify peekToken and popToken so that they use `lift`.
-- I will be able to use throwError and catchError from MonadError.

peekToken :: Parser (Maybe ST.ScannedToken)
peekToken = state $ \ts -> (listToMaybe ts, ts)
popToken :: Parser (Maybe ST.ScannedToken)
popToken = state $ \ts -> (listToMaybe ts, tail ts)

-- | Runs given parser on provided tokens and returns parsed expression and remaining tokens.
runParser :: Parser a -> [ST.ScannedToken] -> (a, [ST.ScannedToken])
runParser parser scannedTokens = runState parser scannedTokens


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
                    _ -> error "Expected right parentheses after expression."
            _ -> error "Expected primary expression"
        Nothing -> error "Expected primary expression"

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
