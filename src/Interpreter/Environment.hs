module Interpreter.Environment
    ( Environment
    , setVar
    , getVar
    , empty
    ) where

import qualified Data.HashMap.Strict as M
import Interpreter.Value (Value)

data Environment = Environment
    { _values :: M.HashMap String Value
    }

-- TODO: Should I use lenses here, to make this shorter?
setVar :: Environment -> String -> Value -> Environment
setVar env name value = env { _values = M.insert name value (_values env) }

getVar :: Environment -> String -> Maybe Value
getVar env name = M.lookup name (_values env)

empty :: Environment
empty = Environment { _values = M.empty }
