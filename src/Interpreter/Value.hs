module Interpreter.Value
    ( Value(..)
    , isTruthy
    ) where

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

isTruthy :: Value -> Bool
isTruthy (BooleanValue False) = False
isTruthy NilValue = False
isTruthy _ = True
