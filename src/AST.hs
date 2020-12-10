module AST
  ( Stmt(..)
  , Expr(..)
  , Literal(..)
  , UnaryOperator(..)
  , BinaryOperator(..)
  ) where

import Data.List (intercalate)

-- Parameter "c" stands for context -> idea is that you can add context/metadata to any node in the AST.
-- While it could be anything, normally it will be information about the location in source to
-- which the AST node corresponds, where it was parsed from, which is then later used for error reporting.
-- Normally parser creates AST, so parser will dictate the type of context.

data Stmt c = ExprStmt (Expr c)
            | PrintStmt c (Expr c)
            | VarStmt c Identifier (Maybe (Expr c)) -- ^ variable name, maybe initializer.
            | BlockStmt c [Stmt c]
            | IfStmt c (Expr c) (Stmt c) (Maybe (Stmt c))  -- ^ condition, thenBranch, maybe elseBranch.
            | WhileStmt c (Expr c) (Stmt c) -- ^ condition, body.

data Expr c = LiteralExpr c Literal
            | UnaryOperatorExpr c UnaryOperator (Expr c)
            | BinaryOperatorExpr c BinaryOperator (Expr c) (Expr c)
            | GroupingExpr c (Expr c)
            | VariableExpr c Identifier -- ^ variable name.
            | AssignExpr c Identifier (Expr c) -- ^ variable name, expression to assign.

type Identifier = String

data Literal = StringLiteral String
             | NumberLiteral Double
             | BooleanLiteral Bool
             | NilLiteral

data UnaryOperator = UnaryMinus | Not

data BinaryOperator = Equal
                    | NotEqual
                    | Less
                    | LessEqual
                    | Greater
                    | GreaterEqual
                    | Plus
                    | Minus
                    | Star
                    | Slash
                    | Or
                    | And

instance Show (Expr c) where
    show (LiteralExpr _ literal) = show literal
    show (UnaryOperatorExpr _ op e) = parenthesize [show op, show e]
    show (BinaryOperatorExpr _ op lhe rhe) = parenthesize [show lhe, show op, show rhe]
    show (GroupingExpr _ e) = parenthesize ["group", show e]
    show (VariableExpr _ identifier) = identifier
    show (AssignExpr _ identifier e) = parenthesize [identifier, "=", show e]

instance Show Literal where
    show (StringLiteral string) = "\"" ++ string ++ "\""
    show (NumberLiteral number) = show number
    show (BooleanLiteral boolean) = if boolean then "true" else "false"
    show NilLiteral = "nil"

instance Show UnaryOperator where
    show UnaryMinus = "-"
    show Not = "!"

instance Show BinaryOperator where
    show Equal = "=="
    show NotEqual = "!="
    show Less = "<"
    show LessEqual = "<="
    show Greater = ">"
    show GreaterEqual = ">="
    show Plus = "+"
    show Minus = "-"
    show Star = "*"
    show Slash = "/"
    show Or = "or"
    show And = "and"

parenthesize :: [String] -> String
parenthesize xs = "(" ++ (intercalate " " xs) ++ ")"
