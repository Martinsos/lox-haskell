module Lib
    ( run
    ) where

import System.IO (hPutStrLn, stderr)
import qualified Scanner as S


run :: String -> IO ()
run source = do
  let (scannerErrors, scannedTokens) = S.scanTokens source
  sequence_ $ map (putStrLn . show) scannedTokens
  sequence_ $ map (reportScannerError) scannerErrors

reportScannerError :: S.ScannerError -> IO ()
reportScannerError (S.ScannerError msg line) = hPutStrLn stderr $ "[line " ++ (show line) ++ "] Error: " ++ msg
