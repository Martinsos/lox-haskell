module Parser
    ( parse
    ) where

import qualified ScannedToken as ST
import qualified AST
import TokenParser (ParseError(..), runParser)
import Parser.Stmt (stmts)
import Parser.ASTContext (Context)


parse :: [ST.ScannedToken] -> (Maybe [AST.Stmt Context], [ParseError], [ST.ScannedToken])
parse = runParser stmts
