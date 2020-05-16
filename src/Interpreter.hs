module Interpreter
    ( evalExpr
    , RuntimeError(..)
    ) where

import qualified AST
import Control.Monad.Except (throwError)

data Value = StringValue String
           | BooleanValue Bool
           | NumberValue Double
           | NilValue
    deriving (Eq)

instance Show Value where
    show (StringValue v) = "\"" ++ v ++ "\""
    show (BooleanValue v) = if v then "true" else "false"
    show (NumberValue v) = show v
    show NilValue = "nil"

-- TODO: Next is sub-chapter 7.3: Runtime Errors.
--   We want to report token at which runtime error happened. However, that is going to be hard,
--   since I did not pass token information into AST! So I have no idea which token is which operator coming from.
--   I will have to do some refactoring of AST in order to support this.

type Result = Either RuntimeError Value

data RuntimeError = RuntimeError { _errorMsg :: String }

throwRuntimeError :: String -> Result
throwRuntimeError errorMsg = throwError $ RuntimeError { _errorMsg = errorMsg }

evalExpr:: AST.Expr -> Result
evalExpr expr = case expr of
    AST.LiteralExpr literal -> evalLiteral literal
    AST.UnaryOperatorExpr op e -> evalUnaryOperation op e
    AST.BinaryOperatorExpr op lhe rhe -> evalBinaryOperation op lhe rhe
    AST.GroupingExpr e -> evalExpr e

evalLiteral :: AST.Literal -> Result
evalLiteral literal = return $ case literal of
    AST.StringLiteral v -> StringValue v
    AST.BooleanLiteral v -> BooleanValue v
    AST.NumberLiteral v -> NumberValue v
    AST.NilLiteral -> NilValue

evalUnaryOperation :: AST.UnaryOperator -> AST.Expr -> Result
evalUnaryOperation operator operandExpr = do
    operandValue <- evalExpr operandExpr
    case operator of
        AST.UnaryMinus -> case operandValue of
            NumberValue number -> return $ NumberValue (-number)
            _ -> throwRuntimeError "Unary operator - expected number."
        AST.Not -> return $ BooleanValue $ not $ isTruthy operandValue

evalBinaryOperation :: AST.BinaryOperator -> AST.Expr -> AST.Expr -> Result
evalBinaryOperation operator lhExpr rhExpr = do
    -- NOTE: In book's Java implementation, there is clear order of evaluation:
    --   first left expression is evaluated, then the right one. Author says this is important,
    --   explicit part of implementation that should be part of language specification.
    --   However, I am not sure how to ensure that here! I am pretty sure right now there is no
    --   order, since it is all lazy and order can be anything.
    --   I wonder, should I use something like deepseq or seq or pseq to ensure the order?
    --   From what I read, pseq should be able to ensure order, but still I am not sure if that is the way.
    --   Does it make any sense to worry about this at all, since there is no IO involved?
    a <- evalExpr lhExpr
    b <- evalExpr rhExpr

    case operator of
        AST.Minus -> evalArithmeticOperation (-) a b
        AST.Slash -> evalArithmeticOperation (/) a b
        AST.Star -> evalArithmeticOperation (*) a b

        AST.Plus -> case (a, b) of
            (NumberValue na, NumberValue nb) -> return $ NumberValue $ na + nb
            (StringValue sa, StringValue sb) -> return $ StringValue $ sa ++ sb
            _ -> throwRuntimeError "Operator + expected two numbers or two strings."

        AST.Greater -> evalComparisonOperation (>) a b
        AST.GreaterEqual -> evalComparisonOperation (>=) a b
        AST.Less -> evalComparisonOperation (<) a b
        AST.LessEqual -> evalComparisonOperation (<=) a b

        AST.Equal -> return $ BooleanValue $ a == b
        AST.NotEqual -> return $ BooleanValue $ a /= b
  where
    evalArithmeticOperation :: (Double -> Double -> Double) -> Value -> Value -> Result
    evalArithmeticOperation f a b = case (a, b) of
        (NumberValue na, NumberValue nb) -> return $ NumberValue $ f na nb
        _ -> throwRuntimeError "Binary arithmetic operator expected two numbers!"

    evalComparisonOperation :: (Double -> Double -> Bool) -> Value -> Value -> Result
    evalComparisonOperation f a b = case (a, b) of
        (NumberValue na, NumberValue nb) -> return $ BooleanValue $ f na nb
        _ -> throwRuntimeError "Binary comparison operator expected two numbers!"

isTruthy :: Value -> Bool
isTruthy (BooleanValue False) = False
isTruthy NilValue = False
isTruthy _ = True
