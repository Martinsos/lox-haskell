module Parser.Stmt
    ( stmts
    ) where

import qualified ScannedToken as ST
import qualified Token as T
import TokenParser (Parser, popToken, handleToken, catchError)
import qualified AST
import qualified Parser.ASTContext as C
import qualified Parser.Expr as PE
import Parser.Utils (ifToken, consumeToken, consumeIdentifierToken)

type StmtParser  = Parser ST.ScannedToken (AST.Stmt C.Context)
type StmtsParser = Parser ST.ScannedToken [AST.Stmt C.Context]


stmts :: StmtsParser
stmts = handleToken
    (\_ -> do
            maybeParsedStmt <- declaration
            parsedStmts <- stmts
            return $ maybe parsedStmts (:parsedStmts) maybeParsedStmt)
    (return [])

-- NOTE: Although the name of this function is `declaration`, it does not parse only declaration statements,
--   actually it tries to parse declaration stmt but if that fails it tries to parse any other statement.
--   This can seem confusing, but this is because organization and naming of functions in parser does not
--   reflect the AST, instead it reflects the grammar rules from the CI book, and those are named by
--   precedence, not by hierarchy.
-- | If error occurs, synchronization happens and no statement is returned.
-- If there is no error, statement is returned.
declaration :: Parser ST.ScannedToken (Maybe (AST.Stmt C.Context))
declaration = (Just <$> (ifToken (== T.Var) (const varDeclaration) stmt))
              `catchError` (\_ -> synchronize >> return Nothing)

-- | Parses variable declaration, otherwise throws parser error.
varDeclaration :: StmtParser
varDeclaration = do
    varToken <- consumeToken (== T.Var) "Expected 'var' at start of a variable declaration."
    (_, identifierLiteral) <- consumeIdentifierToken
    maybeInitializerExpr <- ifToken (== T.Equal)
                                    (\_ -> popToken >> Just <$> PE.expression)
                                    (return Nothing)
    _ <- consumeToken (== T.Semicolon) "Expected ';' after variable declaration."
    return $ AST.VarStmt (C.withToken varToken) identifierLiteral maybeInitializerExpr

stmt :: StmtParser
stmt = ifToken (== T.Print) (const printStmt) exprStmt

printStmt :: StmtParser
printStmt = do
    printToken <- consumeToken (== T.Print) "Expected print statement."
    expr <- PE.expression
    _ <- consumeToken (== T.Semicolon) "Expected ';' at the end of print statement."
    return $ AST.PrintStmt (C.withToken printToken) expr

exprStmt :: StmtParser
exprStmt = do
    expr <- PE.expression
    _ <- consumeToken (== T.Semicolon) "Expected ';' at the end of expression statement."
    return $ AST.ExprStmt expr

-- | Pops tokens until it encounters end of the statement or start of the statement.
synchronize :: Parser ST.ScannedToken ()
synchronize = handleToken
    (\st -> case ST._token st of
                T.Semicolon         -> popToken >> return ()
                t | isStartOfStmt t -> return ()
                _                   -> popToken >> synchronize)
    (return ())
  where
    isStartOfStmt :: T.Token -> Bool
    isStartOfStmt t = t `elem` [T.Class, T.Fun, T.Var, T.For, T.If, T.While, T.Print, T.Return]
