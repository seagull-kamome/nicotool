{- -}
module Utils.Browser.GoogleChrome (cookieLoader) where

import Control.Monad
import Control.Exception
import System.Directory
import Control.Monad.Trans
import Utils.Browser
import Data.List.Split (splitOn)
import Data.List (intersperse)
import Database.HDBC
import Database.HDBC.Sqlite3

--
-- .firefox/default/xxxx/cookies.sqlite 
-- 
cookieLoader :: MonadIO m => CookieLoader m
cookieLoader domain = liftIO $ do
  dbpath <- getHomeDirectory >>= return . flip (++) "/.config/google-chrome/Default/Cookies"
  exists <- doesFileExist dbpath
  when (not exists) $ error "No database found"
  bracket
    (connectSqlite3 dbpath)
    disconnect
    (\db ->
      do
        stmt <- prepare db sql
        execute stmt $ map toSql pat
        fetchAllRows stmt >>= 
          (mapM (\[name, value] -> evaluate $ (fromSql name, fromSql value)))
    )
  where
    pat = foldl (\xs y -> ("." ++ y ++ if null xs then "" else head xs):xs) [] $ reverse $ splitOn "." domain
    sql = "SELECT name, value FROM cookies WHERE " ++
          (concat $ intersperse " OR " $ replicate (length pat) "host_key=?") ++ " ORDER BY expires_utc DESC"

-- main :: IO ()
-- main = cookieLoader "live.nicovideo.jp" >>= print



