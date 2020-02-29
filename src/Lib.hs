module Lib
    ( run
    -- Exported for the purpose of tests:
    , scanTokens
    ) where

run :: String -> IO ()
run source = do
  let tokens = scanTokens source
  sequence_ $ map putStrLn tokens

scanTokens :: String -> [String]
scanTokens source = concat $ map words $ lines $ source
