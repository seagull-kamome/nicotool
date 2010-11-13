{- -}
module Utils.Browser (CookieLoader) where

import Control.Monad.Trans
import Network.HTTP.Cookie

type CookieLoader m = String -> m [Cookie]


