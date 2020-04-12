module AST.RpnPrinter
  ( printRpn
  ) where

import Data.List
import AST

-- This module implements Reverse Polish Notation printer, which was additional challenge in the book.

printRpn :: Expr -> String
printRpn (LiteralExpr literal) = show literal
printRpn (UnaryOperatorExpr op e) = intercalate " " [printRpn e, show op]
printRpn (BinaryOperatorExpr op lhe rhe) = intercalate " " [printRpn lhe, printRpn rhe, show op]
printRpn (GroupingExpr e) = printRpn e  -- I guess I can just ignore grouping?
