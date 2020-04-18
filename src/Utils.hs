module Utils
    ( safeHead
    ) where

import Data.Maybe (listToMaybe)

safeHead :: [a] -> Maybe a
safeHead = listToMaybe
