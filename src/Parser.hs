module Parser
    ( expression
    ) where

import qualified ScannedToken as ST
import qualified Token as T
import qualified AST

-- | Takes list of tokens, parses some of them into expression and returns
-- that expression together with remaining tokens.
type ExprParser = [ST.ScannedToken] -> ([ST.ScannedToken], AST.Expr)

expression :: ExprParser
expression = equality

equality :: ExprParser
equality = makeBinaryOperationParser [(T.EqualEqual, AST.Equal), (T.BangEqual, AST.NotEqual)] comparison

comparison :: ExprParser
comparison = makeBinaryOperationParser operators addition
  where operators = [ (T.Less, AST.Less)
                    , (T.LessEqual, AST.LessEqual)
                    , (T.Greater, AST.Greater)
                    , (T.GreaterEqual, AST.GreaterEqual)
                    ]

addition :: ExprParser
addition = makeBinaryOperationParser [(T.Minus, AST.Minus), (T.Plus, AST.Plus)] multiplication

multiplication :: ExprParser
multiplication = makeBinaryOperationParser [(T.Star, AST.Star), (T.Slash, AST.Slash)] unary

unary :: ExprParser
unary (st:sts) | ST._token st == T.Bang  = parseUnary AST.Not
               | ST._token st == T.Minus = parseUnary AST.UnaryMinus
  where parseUnary operator = let (sts', subExpr) = unary sts
                              in (sts', AST.UnaryOperatorExpr operator subExpr)
unary sts = primary sts

primary :: ExprParser
primary (st:sts) = case ST._token st of
    T.False -> (sts, AST.LiteralExpr $ AST.BooleanLiteral False)
    T.True  -> (sts, AST.LiteralExpr $ AST.BooleanLiteral True)
    T.Nil   -> (sts, AST.LiteralExpr AST.NilLiteral)
    (T.Number _ value) -> (sts, AST.LiteralExpr $ AST.NumberLiteral value)
    (T.String _ value) -> (sts, AST.LiteralExpr $ AST.StringLiteral value)
    T.LeftParen -> let (sts', subExpr) = expression sts
                   in if (null sts' || (ST._token $ head sts') /= T.RightParen)
                         -- TODO (in subchapter 6.3 -> Handle this error appropriatelly!)
                      then error "Expected right parentheses after expression."
                      else (tail sts', AST.GroupingExpr subExpr)
    _ -> (sts, error "Expected primary expression")
primary sts = (sts, error "Expected primary expression")

-- | Returns parser that will parse tokens based on the `subExprParser ((op1 | op2 | ... | op2) subExprParser)*`
-- grammar production.
-- If multiple operators are provided, it works for a class of binary operators.
makeBinaryOperationParser
    :: [(T.Token, AST.BinaryOperator)] -- * Valid binary operators tokens and their corresponding AST operators.
    -> ExprParser -- * Sub-expression parser.
    -> ExprParser -- * Either a binary operator expression or, if there is none, just sub-expression.
makeBinaryOperationParser operators subExprParser = \scannedTokens ->
    let (scannedTokens', lhe) = subExprParser scannedTokens
    in tryParsingRhe lhe scannedTokens'
  where
    -- | Given lhe expression, returns parser that tries to parse the following binary operator and rhe expression.
    -- If there is no binary operator following, it just returns the given lhe expression.
    tryParsingRhe :: AST.Expr -> ExprParser
    tryParsingRhe lhe (st:sts) | (Just operator) <- lookup (ST._token st) operators =
                                   let (sts', rhe) = subExprParser sts
                                       lhe' = AST.BinaryOperatorExpr operator lhe rhe
                                   in tryParsingRhe lhe' sts'
    tryParsingRhe lhe sts = (sts, lhe)
