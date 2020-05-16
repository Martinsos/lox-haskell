module Parser
    ( parse
    ) where

import qualified ScannedToken as ST
import qualified AST
import TokenParser (ParseError(..), runParser)
import Parser.Expr (expression)
import Parser.ASTContext (Context)


parse :: [ST.ScannedToken] -> (Maybe (AST.Expr Context), [ParseError], [ST.ScannedToken])
parse = runParser expression
