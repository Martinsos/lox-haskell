module Lib
    ( run
    ) where

import System.IO (hPutStrLn, stderr)
import qualified Scanner as S


run :: String -> IO ()
run source = do
  let (parseErrors, parsedTokens) = S.scanTokens source
  sequence_ $ map (putStrLn . show) parsedTokens
  sequence_ $ map (reportParseError) parseErrors

reportParseError :: S.ParseError -> IO ()
reportParseError (S.ParseError msg line) = hPutStrLn stderr $ "[line " ++ (show line) ++ "] Error: " ++ msg
