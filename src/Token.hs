module Token
  ( Token(..)
  ) where

-- NOTE: In book, this type is called TokenType.
--   I put values of literals directly into it, so I named it Token.
data Token =
  -- Single character tokens.
  LeftParen | RightParen | LeftBrace | RightBrace
  | Comma | Dot | Minus | Plus | Semicolon | Slash | Star

  -- One or two character tokens.
  | Bang | BangEqual | Equal | EqualEqual | Greater | GreaterEqual
  | Less | LessEqual

  -- Literals.
  | Identfier String | String String | Number Double

  -- Keywords.
  | And | Class | Else | False | Fun | For | If | Nil | Or
  | Print | Return | Super | This | True | Var | While

  | Eof
  deriving (Show, Eq)
