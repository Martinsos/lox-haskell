module AST
  ( Expr(..)
  , Literal(..)
  , UnaryOperator(..)
  , BinaryOperator(..)
  ) where

import Data.List (intercalate)

-- NOTE: While book author used Visitor in Java to solve the Expression Problem,
-- I did not solve it here because solutions I found in Haskell for it just felt like they
-- would bring more complexity than not solving it at all.
-- NOTE: While book author used Object to present literals, and Token for unary/binary operators,
-- I could not use smth like Objects since we are in Haskell, so I defined Literal explicitely,
-- and since I already did that, I also used new data types for unary/binary operators,
-- which feels more "haskellish" and it also gives us more type safety.
-- This might force us to do some validation/checks earlier though, so let's see how this will
-- impact further development.
-- TODO: There is some repetition of logic between AST and Token, when defining literals and operators.
--   It might be a good idea to remove this duplication, but I don't want to rush with it yet.

data Expr = LiteralExpr Literal
          | UnaryOperatorExpr UnaryOperator Expr
          | BinaryOperatorExpr BinaryOperator Expr Expr
          | GroupingExpr Expr

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

instance Show Expr where
  show (LiteralExpr literal) = show literal
  show (UnaryOperatorExpr op e) = parenthesize [show op, show e]
  show (BinaryOperatorExpr op lhe rhe) = parenthesize [show op, show lhe, show rhe]
  show (GroupingExpr e) = parenthesize ["group", show e]

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

parenthesize :: [String] -> String
parenthesize xs = "(" ++ (intercalate " " xs) ++ ")"
