module Parser.Expr
    ( expression
    ) where

import qualified ScannedToken as ST
import qualified Token as T
import TokenParser (Parser, ParseError(..), Position(..), peekToken, popToken, logAndThrowError)
import qualified AST
import qualified Parser.ASTContext as C

type ExprParser = Parser ST.ScannedToken (AST.Expr C.Context)

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
        Just t | ST._token t == T.Bang  -> popToken >> parseUnary t AST.Not
               | ST._token t == T.Minus -> popToken >> parseUnary t AST.UnaryMinus
        _ -> primary
  where parseUnary token operator = do
            subExpr <- unary
            return $ AST.UnaryOperatorExpr (C.withToken token) operator subExpr

-- | NUMBER | STRING | "false" | "true" | "nil" | "(" expression ")"
primary :: ExprParser
primary = do
    maybeToken <- peekToken
    case maybeToken of
        Just t ->
            let consumeTokenAsLiteral literal = popToken >> (return $ AST.LiteralExpr (C.withToken t) literal)
            in case ST._token t of
                   T.False            -> consumeTokenAsLiteral $ AST.BooleanLiteral False
                   T.True             -> consumeTokenAsLiteral $ AST.BooleanLiteral True
                   T.Nil              -> consumeTokenAsLiteral AST.NilLiteral
                   (T.Number _ value) -> consumeTokenAsLiteral $ AST.NumberLiteral value
                   (T.String _ value) -> consumeTokenAsLiteral $ AST.StringLiteral value
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
    tryParsingRhe :: AST.Expr C.Context -> ExprParser
    tryParsingRhe lhe = do
        maybeToken <- peekToken
        case maybeToken of
            Just t -> case (lookup (ST._token t) operators) of
                Just operator -> do
                    _ <- popToken
                    rhe <- subExprParser
                    let lhe' = AST.BinaryOperatorExpr (C.withToken t) operator lhe rhe
                    tryParsingRhe lhe'
                Nothing -> return lhe
            Nothing -> return lhe

-- | Pops tokens until it encounters end of the statement or start of the statement.
synchronize :: Parser ST.ScannedToken ()
synchronize = do
    nextScannedToken <- peekToken
    case nextScannedToken of
        Just st -> case ST._token st of
            T.Semicolon         -> popToken >> return ()
            t | isStartOfStmt t -> return ()
            _                   -> popToken >> synchronize
        Nothing -> return ()
  where
    isStartOfStmt :: T.Token -> Bool
    isStartOfStmt t = t `elem` [T.Class, T.Fun, T.Var, T.For, T.If, T.While, T.Print, T.Return]
