module Interpreter
    ( evalExpr
    , RuntimeError(..)
    ) where

import qualified AST
import qualified Parser.ASTContext as C
import Control.Monad.Except (throwError)
import qualified ScannedToken as ST

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

type Result = Either RuntimeError Value

data RuntimeError = RuntimeError { _errorMsg :: String, _errorLine :: Maybe Int }

throwRuntimeError :: String -> C.Context -> Result
throwRuntimeError msg context = throwError $ RuntimeError { _errorMsg = msg, _errorLine = line }
  where line = C._token context >>= return . ST._line

evalExpr :: AST.Expr C.Context -> Result
evalExpr (AST.LiteralExpr c literal) = evalLiteral c literal
evalExpr (AST.UnaryOperatorExpr c op expr) = evalUnaryOperation c op expr
evalExpr (AST.BinaryOperatorExpr c op lExpr rExpr) = evalBinaryOperation c op lExpr rExpr
evalExpr (AST.GroupingExpr _ expr) = evalExpr expr

evalLiteral :: C.Context -> AST.Literal -> Result
evalLiteral _ literal = return $ case literal of
    AST.StringLiteral v -> StringValue v
    AST.BooleanLiteral v -> BooleanValue v
    AST.NumberLiteral v -> NumberValue v
    AST.NilLiteral -> NilValue

evalUnaryOperation :: C.Context -> AST.UnaryOperator -> AST.Expr C.Context -> Result
evalUnaryOperation context operator expr = do
    operandValue <- evalExpr expr
    case operator of
        AST.UnaryMinus -> case operandValue of
            NumberValue number -> return $ NumberValue (-number)
            _ -> throwRuntimeError "Unary operator (-) expected number." context
        AST.Not -> return $ BooleanValue $ not $ isTruthy operandValue

evalBinaryOperation :: C.Context -> AST.BinaryOperator -> AST.Expr C.Context -> AST.Expr C.Context -> Result
evalBinaryOperation context operator lExpr rExpr = do
    -- NOTE: In book's Java implementation, there is clear order of evaluation:
    --   first left expression is evaluated, then the right one. Author says this is important,
    --   explicit part of implementation that should be part of language specification.
    --   However, I am not sure how to ensure that here! I am pretty sure right now there is no
    --   order, since it is all lazy and order can be anything.
    --   I wonder, should I use something like deepseq or seq or pseq to ensure the order?
    --   From what I read, pseq should be able to ensure order, but still I am not sure if that is the way.
    --   Does it make any sense to worry about this at all, since there is no IO involved?
    a <- evalExpr lExpr
    b <- evalExpr rExpr

    case operator of
        AST.Minus -> evalArithmeticOperation (-) a b
        AST.Slash -> evalArithmeticOperation (/) a b
        AST.Star -> evalArithmeticOperation (*) a b

        AST.Plus -> case (a, b) of
            (NumberValue na, NumberValue nb) -> return $ NumberValue $ na + nb
            (StringValue sa, StringValue sb) -> return $ StringValue $ sa ++ sb
            _ -> throwRuntimeError "Operator + expected two numbers or two strings." context

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
        _ -> throwRuntimeError "Binary arithmetic operator expected two numbers!" context

    evalComparisonOperation :: (Double -> Double -> Bool) -> Value -> Value -> Result
    evalComparisonOperation f a b = case (a, b) of
        (NumberValue na, NumberValue nb) -> return $ BooleanValue $ f na nb
        _ -> throwRuntimeError "Binary comparison operator expected two numbers!" context

isTruthy :: Value -> Bool
isTruthy (BooleanValue False) = False
isTruthy NilValue = False
isTruthy _ = True
