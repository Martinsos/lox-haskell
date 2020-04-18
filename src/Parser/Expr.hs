module Parser.Expr
    ( expression
    ) where

import qualified ScannedToken as ST
import qualified Token as T
import qualified AST

import Parser (Parser, ParseError(..), Position(..), peekToken, popToken, logAndThrowError)

type ExprParser = Parser ST.ScannedToken AST.Expr

expression :: ExprParser
expression = equality

-- | equality -> comparison ( ("!=" | "==" ) comparison )*
equality :: ExprParser
equality = makeBinaryOperationParser [(T.EqualEqual, AST.Equal), (T.BangEqual, AST.NotEqual)] comparison

-- | comparison -> addition ( (">" | ">=" | "<" | "<=" ) addition )*
comparison :: ExprParser
comparison = makeBinaryOperationParser operators addition
  where operators = [ (T.Less, AST.Less)
                    , (T.LessEqual, AST.LessEqual)
                    , (T.Greater, AST.Greater)
                    , (T.GreaterEqual, AST.GreaterEqual)
                    ]

-- | addition -> multiplication ( ("-" | "+" ) multiplication )*
addition :: ExprParser
addition = makeBinaryOperationParser [(T.Minus, AST.Minus), (T.Plus, AST.Plus)] multiplication

-- | multiplication -> unary ( ("*" | "/" ) unary )*
multiplication :: ExprParser
multiplication = makeBinaryOperationParser [(T.Star, AST.Star), (T.Slash, AST.Slash)] unary

-- | unary -> ( "!" | "-" ) unary | primary
unary :: ExprParser
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
primary :: ExprParser
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
    -> ExprParser -- * Sub-expression parser.
    -> ExprParser -- * Either a binary operator expression or, if there is none, just sub-expression.
makeBinaryOperationParser operators subExprParser = do
    lhe <- subExprParser
    tryParsingRhe lhe
  where
    -- | Given lhe expression, returns parser that tries to parse the following binary operator and rhe expression.
    -- If there is no binary operator following, it just returns the given lhe expression.
    tryParsingRhe :: AST.Expr -> ExprParser
    tryParsingRhe lhe = do
        maybeToken <- peekToken
        case maybeToken >>= \t -> lookup (ST._token t) operators of
            Just operator -> do
                _ <- popToken
                rhe <- subExprParser
                let lhe' = AST.BinaryOperatorExpr operator lhe rhe
                tryParsingRhe lhe'
            Nothing -> return lhe
