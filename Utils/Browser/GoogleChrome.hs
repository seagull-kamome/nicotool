module Utils.Brower.GoogleChrome (cookieLoader) where

import Control.Monad
import Control.Exception
import System.Directory
import Network.HTTP.Cookie
import Control.Monad.Trans
import Utils.Browser
import Data.List.Split (splitOn)
import Data.List (intersperse)
import Database.HDBC
import Database.HDBC.Sqlite3

--
-- .firefox/default/xxxx/cookies.sqlite 
-- 
cookieLoader :: MonadIO m => FilePath -> CookieLoader m
cookieLoader dbpath domain = liftIO $ do
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
               evaluate . (map (\(host:path:isSecure:expiry:name:value:[]) ->
                                MkCookie (fromSql host) (fromSql name) (fromSql value) (Just $ fromSql path) Nothing Nothing))
      )
    where
        pat = foldl (\xs y -> ("." ++ y ++ if null xs then "" else head xs):xs) [] $ reverse $ splitOn "." domain
        sql = "SELECT host, path, isSecure, expiry, name, value FROM moz_cookies WHERE " ++
              (concat $ intersperse " OR " $ replicate (length pat) "host=?") ++ " ORDER BY expiry DESC"

