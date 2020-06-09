module Parser.Expr
    ( expression
    ) where

import qualified ScannedToken as ST
import qualified Token as T
import TokenParser (Parser, popToken, handleToken, logAndThrowError, logError)
import qualified AST
import qualified Parser.ASTContext as C
import Parser.Utils (consumeToken, ifToken, errorAtToken, errorAtEof)

type ExprParser = Parser ST.ScannedToken (AST.Expr C.Context)

expression :: ExprParser
expression = assignment

assignment :: ExprParser
assignment = do
    expr <- equality
    ifToken
        (== T.Equal)
        (\equalToken -> do
            value <- assignment
            case expr of
                AST.VariableExpr _ identifier -> return $ AST.AssignExpr (C.withToken equalToken) identifier value
                _ -> logError (errorAtToken "Invalid assignment target." equalToken) >> return expr
        )
        (return expr)

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
unary = handleToken
    (\t -> case ST._token t of
               T.Bang  -> popToken >> parseUnary t AST.Not
               T.Minus -> popToken >> parseUnary t AST.UnaryMinus
               _ -> primary)
    primary
  where parseUnary token operator = do
            subExpr <- unary
            return $ AST.UnaryOperatorExpr (C.withToken token) operator subExpr

-- | NUMBER | STRING | "false" | "true" | "nil" | "(" expression ")" | IDENTIFIER
primary :: ExprParser
primary = handleToken
    (\t -> let consumeTokenAsLiteral literal = popToken >> (return $ AST.LiteralExpr (C.withToken t) literal)
           in case ST._token t of
                  T.False            -> consumeTokenAsLiteral $ AST.BooleanLiteral False
                  T.True             -> consumeTokenAsLiteral $ AST.BooleanLiteral True
                  T.Nil              -> consumeTokenAsLiteral AST.NilLiteral
                  (T.Number _ value) -> consumeTokenAsLiteral $ AST.NumberLiteral value
                  (T.String _ value) -> consumeTokenAsLiteral $ AST.StringLiteral value
                  (T.Identifier value) -> popToken >> (return $ AST.VariableExpr (C.withToken t) value)
                  T.LeftParen        -> popToken >> do
                      subExpr <- expression
                      _ <- consumeToken (== T.RightParen) "Expected ')'"
                      return subExpr
                  _ -> logAndThrowError (errorAtToken "Expected primary expression." t))
    (logAndThrowError (errorAtEof "Expected primary expression."))

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
    tryParsingRhe lhe = handleToken
        (\t -> case (lookup (ST._token t) operators) of
                   Just operator -> do
                       _ <- popToken
                       rhe <- subExprParser
                       let lhe' = AST.BinaryOperatorExpr (C.withToken t) operator lhe rhe
                       tryParsingRhe lhe'
                   Nothing -> return lhe)
        (return lhe)
