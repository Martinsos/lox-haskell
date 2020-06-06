module Interpreter
    ( interpret
    , RuntimeError(..) ) where

import qualified AST
import qualified Parser.ASTContext as C
import Control.Monad.IO.Class (liftIO)
import Interpreter.Value (Value(..), isTruthy)
import Interpreter.Core (Interpreter, runInterpreter, RuntimeError(..), throwRuntimeError, defineVar, getVar)

type Program = [AST.Stmt C.Context]

interpret :: Program -> IO (Either RuntimeError ())
interpret stmts = runInterpreter $ mapM_ evalStmt stmts

evalExpr :: AST.Expr C.Context -> Interpreter Value
evalExpr (AST.LiteralExpr c literal) = evalLiteral c literal
evalExpr (AST.UnaryOperatorExpr c op expr) = evalUnaryOperation c op expr
evalExpr (AST.BinaryOperatorExpr c op lExpr rExpr) = evalBinaryOperation c op lExpr rExpr
evalExpr (AST.GroupingExpr _ expr) = evalExpr expr
evalExpr (AST.VariableExpr c name) = getVar c name

evalLiteral :: C.Context -> AST.Literal -> Interpreter Value
evalLiteral _ literal = return $ case literal of
    AST.StringLiteral v -> StringValue v
    AST.BooleanLiteral v -> BooleanValue v
    AST.NumberLiteral v -> NumberValue v
    AST.NilLiteral -> NilValue

evalUnaryOperation :: C.Context -> AST.UnaryOperator -> AST.Expr C.Context -> Interpreter Value
evalUnaryOperation context operator expr = do
    operandValue <- evalExpr expr
    case operator of
        AST.UnaryMinus -> case operandValue of
            NumberValue number -> return $ NumberValue (-number)
            _ -> throwRuntimeError context "Unary operator (-) expected number."
        AST.Not -> return $ BooleanValue $ not $ isTruthy operandValue

evalBinaryOperation :: C.Context -> AST.BinaryOperator -> AST.Expr C.Context -> AST.Expr C.Context -> Interpreter Value
evalBinaryOperation context operator lExpr rExpr = do
    a <- evalExpr lExpr
    b <- evalExpr rExpr

    case operator of
        AST.Minus -> evalArithmeticOperation (-) a b
        AST.Slash -> evalArithmeticOperation (/) a b
        AST.Star -> evalArithmeticOperation (*) a b

        AST.Plus -> case (a, b) of
            (NumberValue na, NumberValue nb) -> return $ NumberValue $ na + nb
            (StringValue sa, StringValue sb) -> return $ StringValue $ sa ++ sb
            _ -> throwRuntimeError context "Operator + expected two numbers or two strings."

        AST.Greater -> evalComparisonOperation (>) a b
        AST.GreaterEqual -> evalComparisonOperation (>=) a b
        AST.Less -> evalComparisonOperation (<) a b
        AST.LessEqual -> evalComparisonOperation (<=) a b

        AST.Equal -> return $ BooleanValue $ a == b
        AST.NotEqual -> return $ BooleanValue $ a /= b
  where
    evalArithmeticOperation :: (Double -> Double -> Double) -> Value -> Value -> Interpreter Value
    evalArithmeticOperation f a b = case (a, b) of
        (NumberValue na, NumberValue nb) -> return $ NumberValue $ f na nb
        _ -> throwRuntimeError context "Binary arithmetic operator expected two numbers!"

    evalComparisonOperation :: (Double -> Double -> Bool) -> Value -> Value -> Interpreter Value
    evalComparisonOperation f a b = case (a, b) of
        (NumberValue na, NumberValue nb) -> return $ BooleanValue $ f na nb
        _ -> throwRuntimeError context "Binary comparison operator expected two numbers!"

evalStmt :: AST.Stmt C.Context -> Interpreter ()
evalStmt (AST.ExprStmt e) = evalExpr e >> return ()
evalStmt (AST.PrintStmt c e) = evalPrintStmt c e
evalStmt (AST.VarStmt c name initializer) = evalVarStmt c name initializer

evalPrintStmt :: C.Context -> AST.Expr C.Context -> Interpreter ()
evalPrintStmt _ expr = do
    value <- evalExpr expr
    liftIO $ putStrLn $ show value

evalVarStmt :: C.Context -> String -> Maybe (AST.Expr C.Context) -> Interpreter ()
evalVarStmt _ name maybeInitializer = do
    value <- maybe (return NilValue) evalExpr maybeInitializer
    defineVar name value
