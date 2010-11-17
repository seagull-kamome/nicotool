{- -}
module Utils.Browser (CookieLoader) where

import Network.URI

type CookieLoader m = String -> m [(String, String)]

