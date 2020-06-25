{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Interpreter.Core
    ( Interpreter
    , runInterpreter
    , RuntimeError(..)
    , throwRuntimeError
    , getVar
    , setVar
    , assignVar
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (MonadError, ExceptT, runExceptT)
import Control.Monad.State.Lazy (MonadState, StateT, gets, modify, runStateT)
import Control.Monad.Except (throwError)
import qualified Parser.ASTContext as C
import qualified ScannedToken as ST
import qualified Interpreter.Environment as E
import Interpreter.Value (Value)

newtype Interpreter a = Interpreter {
  _runInterpreter :: ExceptT RuntimeError (StateT InterpreterState IO) a
} deriving (Functor, Applicative, Monad, MonadIO, MonadState InterpreterState, MonadError RuntimeError)

runInterpreter :: Interpreter a -> IO (Either RuntimeError a)
runInterpreter interpreter = do
    (resultOrError, _state) <- runStateT (runExceptT $ _runInterpreter interpreter) initialState
    return resultOrError
  where
    initialState = InterpreterState { _environment = E.empty }

data RuntimeError = RuntimeError { _errorMsg :: String, _errorLine :: Maybe Int }

throwRuntimeError :: C.Context -> String -> Interpreter a
throwRuntimeError context msg = throwError $ RuntimeError { _errorMsg = msg, _errorLine = line }
  where line = C._token context >>= return . ST._line

data InterpreterState = InterpreterState
    { _environment :: E.Environment
    }

assignVar :: C.Context -> String -> Value -> Interpreter ()
assignVar context name value = do
    env <- gets _environment
    case E.assignVar env name value of
        Just env' -> modify (\s -> s { _environment = env' })
        Nothing -> throwRuntimeError context ("Undefined variable '" ++ name ++ "'.")

setVar :: String -> Value -> Interpreter ()
setVar name value =
    modify (\s -> s { _environment = E.setVar (_environment s) name value })

getVar :: C.Context -> String -> Interpreter Value
getVar context name = do
    env <- gets _environment
    case E.getVar env name of
        Just value -> return value
        Nothing -> throwRuntimeError context ("Undefined variable '" ++ name ++ "'.")

