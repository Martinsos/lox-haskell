module Parser.ASTContext
    ( Context(..)
    , empty
    , withToken
    ) where

import qualified ScannedToken as ST

data Context = Context
    { _token :: Maybe ST.ScannedToken
    }

empty :: Context
empty = Context { _token = Nothing }

withToken :: ST.ScannedToken -> Context
withToken t = Context { _token = Just t }
