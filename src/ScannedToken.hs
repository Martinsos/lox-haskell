module ScannedToken
  ( ScannedToken(..)
  ) where

import qualified Token as T


-- NOTE: In book, this type is called Token.
data ScannedToken = ScannedToken
  { _token :: T.Token
  , _line :: Int
  } deriving (Eq)

instance Show ScannedToken where
  show pt = (show $ _token pt) ++ " " ++ (show $ _line pt)
