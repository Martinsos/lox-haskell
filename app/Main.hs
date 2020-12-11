module Main where

import System.Exit (exitWith, ExitCode(..))
import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.Environment (getArgs)
import qualified Lib

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runPrompt
        1 -> runFile (head args)
        _ -> printUsage >> exitWith exitCodeUsage

exitCodeUsage :: ExitCode
exitCodeUsage = ExitFailure 64 -- As defined in sysexits.h of FreeBSD documentation.

printUsage :: IO ()
printUsage = putStrLn "Usage: hlox [script]"

runPrompt :: IO ()
runPrompt = do
    hSetBuffering stdout NoBuffering
    repl Lib.initState
  where
    repl state = do
        putStr "> "
        source <- getLine
        state' <- Lib.interpret state source
        repl state'

runFile :: FilePath -> IO ()
runFile path = do
    source <- readFile path
    _ <- Lib.interpret Lib.initState source
    return ()
