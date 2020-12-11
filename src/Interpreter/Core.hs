{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Interpreter.Core
    ( Interpreter
    , runInterpreter
    , initState
    , InterpreterState
    , RuntimeError(..)
    , throwRuntimeError
    , getVar
    , setVar
    , assignVar
    , evalScoped
    ) where

import           Control.Monad.Except     (ExceptT, MonadError, catchError,
                                           runExceptT, throwError)
import           Control.Monad.IO.Class   (MonadIO)
import           Control.Monad.State.Lazy (MonadState, StateT, gets, modify,
                                           runStateT)
import           Data.Maybe               (fromJust)

import qualified Interpreter.Environment  as E
import           Interpreter.Value        (Value)
import qualified Parser.ASTContext        as C
import qualified ScannedToken             as ST


newtype Interpreter a = Interpreter {
  _runInterpreter :: ExceptT RuntimeError (StateT InterpreterState IO) a
} deriving (Functor, Applicative, Monad, MonadIO, MonadState InterpreterState, MonadError RuntimeError)

runInterpreter :: InterpreterState -> Interpreter a -> IO (Either RuntimeError a, InterpreterState)
runInterpreter state interpreter =
    runStateT (runExceptT $ _runInterpreter interpreter) state

initState :: InterpreterState
initState = InterpreterState { _environment = E.empty }

data RuntimeError = RuntimeError { _errorMsg :: String, _errorLine :: Maybe Int }

throwRuntimeError :: C.Context -> String -> Interpreter a
throwRuntimeError context msg = throwError $ RuntimeError { _errorMsg = msg, _errorLine = line }
  where line = ST._line <$> C._token context

data InterpreterState = InterpreterState
    { _environment :: E.Environment
    }

assignVar :: C.Context -> String -> Value -> Interpreter ()
assignVar context name value = do
    env <- gets _environment
    case env `E.assignVar` (name, value) of
        Just env' -> setEnv env'
        Nothing -> throwRuntimeError context ("Undefined variable '" ++ name ++ "'.")

setVar :: String -> Value -> Interpreter ()
setVar name value = do
    env <- gets _environment
    let env' = env `E.setVar` (name, value)
    setEnv env'

getVar :: C.Context -> String -> Interpreter Value
getVar context name = do
    env <- gets _environment
    case E.getVar env name of
        Just value -> return value
        Nothing -> throwRuntimeError context ("Undefined variable '" ++ name ++ "'.")

-- | Evaluates given interpreter in the context of a new scope.
-- Scope is closed once evaluation is done, even in case of exception.
evalScoped :: Interpreter a -> Interpreter a
evalScoped toEval = do
    openNewClosure
    result <- toEval `catchError` (\e -> closeNewestClosure >> throwError e)
    closeNewestClosure
    return result
  where
    openNewClosure = gets _environment >>= setEnv . E.enclosed
    closeNewestClosure = gets _environment >>= setEnv . fromJust . E.getEnclosingEnv

setEnv :: E.Environment -> Interpreter ()
setEnv env = modify (\s -> s { _environment = env })
