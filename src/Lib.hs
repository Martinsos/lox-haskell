module Lib
    ( run
    -- Exported for the purpose of tests:
    , scanTokens
    ) where

import System.IO (hPutStrLn, stderr)

run :: String -> IO ()
run source = do
  let tokens = scanTokens source
  sequence_ $ map putStrLn tokens

scanTokens :: String -> [String]
scanTokens source = concat $ map words $ lines $ source

error :: Int -> String -> IO ()
error line message = report line "" message

-- NOTE: In book, he also sets global hadError flag to true here, and uses that to check if error
--   happened after being done with execution of program.
--   I could achieve smth like that right now by introducing Lox (or App) monad instead of IO, but I will leave that
--   for later to see if there is a simpler option.
report :: Int -> String -> String -> IO ()
report line whereMessage message = hPutStrLn stderr $ "[line " ++ (show line) ++ "] Error" ++ whereMessage ++ ": " ++ message
