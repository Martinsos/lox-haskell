module Parser
    ( parse
    ) where

import qualified ScannedToken as ST
import qualified AST
import TokenParser (ParseError(..), runParser)
import Parser.Expr (expression)


parse :: [ST.ScannedToken] -> (Maybe AST.Expr, [ParseError], [ST.ScannedToken])
parse = runParser expression
