{- -}
{-
- nicotool
- Copyright (C) 2020, HATTORI, Hiroki
- HTTPリクエスト用ヘルパ
-
- Released under FreeBSD Licence.
-}
module Network.Service.NicoVideo.HTTP
       (sendRq, sendRqXML) where

import Control.Monad (when)
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Failure
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.UTF8 as U8
import qualified Network.HTTP.Enumerator as NH
import Network.URL
import Utils.Browser
import qualified Text.HTML.TagSoup as TS

import Network.Service.NicoVideo.XML



sendRq :: (MonadIO m) => String -> Maybe (CookieLoader m) ->  m (U8.ByteString)
sendRq url loader = do
  rq <- liftIO $ NH.parseUrl url
  cookie_hdr <- case loader of
    Nothing -> return ""
    Just ldr -> ldr (C8.unpack (NH.host rq)) >>= return . concatMap (\(x,y) -> encString False ok_url x ++ "=" ++ encString False ok_url y ++ ";")
  (NH.Response cd _ bdy) <- liftIO $ NH.httpLbsRedirect
                            (rq { NH.requestHeaders = [ (C8.pack "Cookie", C8.pack cookie_hdr) ]})
  when (cd < 200 || cd >= 300) $ liftIO $ failure $ NH.StatusCodeException cd bdy
  return bdy

sendRqXML :: (MonadIO m) => String -> Maybe (CookieLoader m) ->  TagParserT String u Identity a -> u -> m (Either RequestError a)
sendRqXML url loader parser st = do
  doc <- sendRq url loader >>= return . U8.toString
  let res = runIdentity $ parseTag parser st url $ TS.parseTags $ tail $ dropWhile (/= '\n') doc
  case res of
    Left err -> liftIO (print doc >> print err)
    _ -> return ()
  return res;





