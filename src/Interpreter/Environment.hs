module Interpreter.Environment
    ( Environment
    , setVar
    , assignVar
    , getVar
    , empty
    , enclosed
    , getEnclosingEnv
    ) where

import Control.Monad (mplus)
import qualified Data.HashMap.Strict as M
import Interpreter.Value (Value)

data Environment = Environment
    { _values :: M.HashMap String Value
    , _enclosing :: Maybe Environment -- ^ Parent environment/scope.
    }
    deriving Show

-- TODO: Should I use lenses here, to make this shorter?
-- | Set the var with specified name and value in specificied environment.
-- If such var already existed, it is overwritten.
setVar :: Environment -> String -> Value -> Environment
setVar env name value = env { _values = M.insert name value (_values env) }

-- | If environment contains the variable with specified name, assign it new value and return new Environment.
-- If not, try the same thing for enclosing environment. If none, return Nothing.
assignVar :: Environment -> String -> Value -> Maybe Environment
assignVar env name value = maybeAssignToEnv `mplus` maybeAssignToEnclosingEnv
  where
    maybeAssignToEnv = M.lookup name (_values env) >> return (setVar env name value)
    maybeAssignToEnclosingEnv = do
        enclEnv <- _enclosing env
        enclEnv' <- assignVar enclEnv name value
        return $ env { _enclosing = Just enclEnv' }

-- | If environment contains the variable with specified name, return its value.
-- If not, try the same thing for enclosing environment. If none, return Nothing.
getVar :: Environment -> String -> Maybe Value
getVar env name = maybeValueFromEnv `mplus` maybeValueFromEnclosingEnv
  where
    maybeValueFromEnv = M.lookup name (_values env)
    maybeValueFromEnclosingEnv = _enclosing env >>= \e -> getVar e name

empty :: Environment
empty = Environment { _values = M.empty, _enclosing = Nothing }

-- | Returns new, empty environment, enclosed into given environment.
enclosed :: Environment -> Environment
enclosed env = empty { _enclosing = Just env }

getEnclosingEnv :: Environment -> Maybe Environment
getEnclosingEnv = _enclosing
