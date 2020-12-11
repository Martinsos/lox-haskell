module Lib
    ( interpret
    , I.initState
    , I.InterpreterState
    ) where

import           System.IO   (hPutStrLn, stderr)

import qualified Interpreter as I
import qualified Parser      as P
import qualified Scanner     as S
import qualified TokenParser as TP


interpret :: I.InterpreterState -> String -> IO I.InterpreterState
interpret state source = do
    scannedTokens <- scan source
    maybeAst <- parse scannedTokens
    case maybeAst of
        Nothing  -> return state
        Just ast -> interpretAst state ast
  where
      scan src = do
          let (scannerErrors, scannedTokens) = S.scanTokens src
          reportScannerErrors scannerErrors
          return scannedTokens
        where
            reportScannerErrors errors = mapM_ reportScannerError errors
            reportScannerError (S.ScannerError msg line) = reportError ("line: " ++ show line) msg

      parse scannedTokens = do
          let (maybeAst, parseErrors, _) = P.parse scannedTokens
          reportParseErrors parseErrors
          return maybeAst
        where
            reportParseErrors parseErrors = mapM_ reportParseError parseErrors
            reportParseError (TP.ParseError msg TP.Eof) = reportError "line: EOF" msg
            reportParseError (TP.ParseError msg (TP.LineNumber line)) = reportError ("line: " ++ show line) msg

      interpretAst oldState ast = do
          (errorOrResult, newState) <- I.interpret oldState ast
          case errorOrResult of
              Left runtimeError -> do
                  reportRuntimeError runtimeError
                  return newState
              Right () -> return newState
        where
            reportRuntimeError e = reportError ("line: " ++ maybe "?" show (I._errorLine e)) (I._errorMsg e)

      reportError position msg = hPutStrLn stderr $ "[" ++ position ++ "] Error: " ++ msg
