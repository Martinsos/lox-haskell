module Interpreter
    ( interpret
    , RuntimeError(..) ) where

import qualified AST
import qualified Parser.ASTContext as C
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified ScannedToken as ST

data Value = StringValue !String
           | BooleanValue !Bool
           | NumberValue !Double
           | NilValue
    deriving (Eq)

instance Show Value where
    show (StringValue v) = "\"" ++ v ++ "\""
    show (BooleanValue v) = if v then "true" else "false"
    show (NumberValue v) = show v
    show NilValue = "nil"

type Result a = ExceptT RuntimeError IO a
type ExprResult = Result Value
type StmtResult = Result ()

data RuntimeError = RuntimeError { _errorMsg :: String, _errorLine :: Maybe Int }

type Program = [AST.Stmt C.Context]

interpret :: Program -> IO (Either RuntimeError ())
interpret stmts = runExceptT $ mapM_ evalStmt stmts

evalExpr :: AST.Expr C.Context -> ExprResult
evalExpr (AST.LiteralExpr c literal) = evalLiteral c literal
evalExpr (AST.UnaryOperatorExpr c op expr) = evalUnaryOperation c op expr
evalExpr (AST.BinaryOperatorExpr c op lExpr rExpr) = evalBinaryOperation c op lExpr rExpr
evalExpr (AST.GroupingExpr _ expr) = evalExpr expr

evalLiteral :: C.Context -> AST.Literal -> ExprResult
evalLiteral _ literal = return $ case literal of
    AST.StringLiteral v -> StringValue v
    AST.BooleanLiteral v -> BooleanValue v
    AST.NumberLiteral v -> NumberValue v
    AST.NilLiteral -> NilValue

evalUnaryOperation :: C.Context -> AST.UnaryOperator -> AST.Expr C.Context -> ExprResult
evalUnaryOperation context operator expr = do
    operandValue <- evalExpr expr
    case operator of
        AST.UnaryMinus -> case operandValue of
            NumberValue number -> return $ NumberValue (-number)
            _ -> throwRuntimeError "Unary operator (-) expected number." context
        AST.Not -> return $ BooleanValue $ not $ isTruthy operandValue

evalBinaryOperation :: C.Context -> AST.BinaryOperator -> AST.Expr C.Context -> AST.Expr C.Context -> ExprResult
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
            _ -> throwRuntimeError "Operator + expected two numbers or two strings." context

        AST.Greater -> evalComparisonOperation (>) a b
        AST.GreaterEqual -> evalComparisonOperation (>=) a b
        AST.Less -> evalComparisonOperation (<) a b
        AST.LessEqual -> evalComparisonOperation (<=) a b

        AST.Equal -> return $ BooleanValue $ a == b
        AST.NotEqual -> return $ BooleanValue $ a /= b
  where
    evalArithmeticOperation :: (Double -> Double -> Double) -> Value -> Value -> ExprResult
    evalArithmeticOperation f a b = case (a, b) of
        (NumberValue na, NumberValue nb) -> return $ NumberValue $ f na nb
        _ -> throwRuntimeError "Binary arithmetic operator expected two numbers!" context

    evalComparisonOperation :: (Double -> Double -> Bool) -> Value -> Value -> ExprResult
    evalComparisonOperation f a b = case (a, b) of
        (NumberValue na, NumberValue nb) -> return $ BooleanValue $ f na nb
        _ -> throwRuntimeError "Binary comparison operator expected two numbers!" context

isTruthy :: Value -> Bool
isTruthy (BooleanValue False) = False
isTruthy NilValue = False
isTruthy _ = True

evalStmt :: AST.Stmt C.Context -> StmtResult
evalStmt (AST.ExprStmt e) = evalExpr e >> return ()
evalStmt (AST.PrintStmt c e) = evalPrintStmt c e

evalPrintStmt :: C.Context -> AST.Expr C.Context -> StmtResult
evalPrintStmt _ expr = do
    value <- evalExpr expr
    liftIO $ putStrLn $ show value

throwRuntimeError :: String -> C.Context -> ExprResult
throwRuntimeError msg context = throwError $ RuntimeError { _errorMsg = msg, _errorLine = line }
  where line = C._token context >>= return . ST._line
