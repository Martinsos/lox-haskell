module Lib
    ( run
    ) where

import System.IO (hPutStrLn, stderr)
import qualified Scanner as S
import qualified Parser as P
import qualified TokenParser as TP


run :: String -> IO ()
run source = do
    let (scannerErrors, scannedTokens) = S.scanTokens source
    reportScannerErrors scannerErrors

    let (maybeExpr, parseErrors, _) = P.parse scannedTokens
    reportParseErrors parseErrors
    case maybeExpr of
        Just expr -> putStrLn (show expr)
        Nothing -> putStrLn "Failed to produce the AST."

  where
    reportScannerErrors errors = mapM_ reportScannerError errors
    reportScannerError (S.ScannerError msg line) = reportError ("line: " ++ (show line)) msg

    reportParseErrors parseErrors = mapM_ reportParseError parseErrors
    reportParseError (TP.ParseError msg TP.Eof) = reportError "line: EOF" msg
    reportParseError (TP.ParseError msg (TP.LineNumber line)) = reportError ("line: " ++ (show line)) msg

    reportError position msg = hPutStrLn stderr $ "[" ++ position ++ "] Error: " ++ msg
