module Interpreter
    ( eval
    ) where

import qualified AST

data Value = StringValue String
           | BooleanValue Bool
           | NumberValue Double
           | NilValue
    deriving (Eq)

eval :: AST.Expr -> Value
eval expr = case expr of
    AST.LiteralExpr literal -> evalLiteral literal
    AST.UnaryOperatorExpr op e -> evalUnaryOperation op e
    AST.BinaryOperatorExpr op lhe rhe -> evalBinaryOperation op lhe rhe
    AST.GroupingExpr e -> eval e

evalLiteral :: AST.Literal -> Value
evalLiteral literal = case literal of
    AST.StringLiteral v -> StringValue v
    AST.BooleanLiteral v -> BooleanValue v
    AST.NumberLiteral v -> NumberValue v
    AST.NilLiteral -> NilValue

evalUnaryOperation :: AST.UnaryOperator -> AST.Expr -> Value
evalUnaryOperation operator operandExpr = case operator of
    AST.UnaryMinus -> case operandValue of
        NumberValue number -> NumberValue (-number)
        _ -> error "Can't negate non-number."
    AST.Not -> BooleanValue $ not $ isTruthy operandValue
  where operandValue = eval operandExpr

evalBinaryOperation :: AST.BinaryOperator -> AST.Expr -> AST.Expr -> Value
evalBinaryOperation operator lhExpr rhExpr = case operator of
    AST.Minus -> evalArithmeticOperation (-)
    AST.Slash -> evalArithmeticOperation (/)
    AST.Star -> evalArithmeticOperation (*)

    AST.Plus -> case (lhValue, rhValue) of
        (NumberValue lv, NumberValue rv) -> NumberValue $ lv + rv
        (StringValue lv, StringValue rv) -> StringValue $ lv ++ rv
        _ -> error "Expected numbers or strings!"

    AST.Greater -> evalComparisonOperation (>)
    AST.GreaterEqual -> evalComparisonOperation (>=)
    AST.Less -> evalComparisonOperation (<)
    AST.LessEqual -> evalComparisonOperation (<=)

    AST.Equal -> BooleanValue $ lhValue == rhValue
    AST.NotEqual -> BooleanValue $ lhValue /= rhValue
  where
    -- NOTE: In book's Java implementation, there is clear order of evaluation:
    --   first left expression is evaluated, then the right one. Author says this is important,
    --   explicit part of implementation that should be part of language specification.
    --   However, I am not sure how to ensure that here! I am pretty sure right now there is no
    --   order, since it is all lazy and order can be anything.
    --   I wonder, should I use something like deepseq or seq or pseq to ensure the order?
    --   From what I read, pseq should be able to ensure order, but still I am not sure if that is the way.
    --   Does it make any sense to worry about this at all, since there is no IO involved?
    (lhValue, rhValue) = (eval lhExpr, eval rhExpr)

    evalArithmeticOperation :: (Double -> Double -> Double) -> Value
    evalArithmeticOperation f = case (lhValue, rhValue) of
        (NumberValue lv, NumberValue rv) -> NumberValue $ f lv rv
        _ -> error "Expected numbers!"


    evalComparisonOperation :: (Double -> Double -> Bool) -> Value
    evalComparisonOperation f = case (lhValue, rhValue) of
        (NumberValue lv, NumberValue rv) -> BooleanValue $ f lv rv
        _ -> error "Expected numbers!"

isTruthy :: Value -> Bool
isTruthy (BooleanValue False) = False
isTruthy NilValue = False
isTruthy _ = True
