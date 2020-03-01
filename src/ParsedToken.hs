module ParsedToken
  ( ParsedToken(..)
  ) where

import qualified Token as T

-- NOTE: In book, this type is called Token.
data ParsedToken = ParsedToken
  { _token :: T.Token
  , _lexeme :: String
  , _line :: Int
  } deriving (Eq)

instance Show ParsedToken where
  show pt = (show $ _token pt) ++ " " ++ (_lexeme pt)
