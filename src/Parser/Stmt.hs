module Parser.Stmt
    ( stmts
    ) where

import Data.Maybe (fromJust)
import qualified ScannedToken as ST
import qualified Token as T
import TokenParser (Parser, ParseError(..), Position(..), handleToken, logAndThrowError)
import qualified AST
import qualified Parser.ASTContext as C
import qualified Parser.Expr as PE
import Parser.Utils (expectToken, consumeToken)

type StmtParser  = Parser ST.ScannedToken (AST.Stmt C.Context)
type StmtsParser = Parser ST.ScannedToken [AST.Stmt C.Context]


stmts :: StmtsParser
stmts = handleToken
    (\_ -> do
            s <- stmt
            ss <- stmts
            return $ s:ss)
    (return [])

stmt :: StmtParser
stmt = expectToken T.Print
    (\_ -> printStmt)
    (\_ -> exprStmt)
    (logAndThrowError (ParseError "Expected start of statement." Eof))

printStmt :: StmtParser
printStmt = do
    maybePrintToken <- consumeToken T.Print "Expected print statement."
    expr <- PE.expression
    _ <- consumeToken T.Semicolon "Expected semicolon at the end of print statement."
    return $ AST.PrintStmt (C.withToken (fromJust maybePrintToken)) expr

exprStmt :: StmtParser
exprStmt = do
    expr <- PE.expression
    _ <- consumeToken T.Semicolon "Expected semicolon at the end of expression statement."
    return $ AST.ExprStmt expr

