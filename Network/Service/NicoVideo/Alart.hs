{-# LANGUAGE RecordWildCards, NamedFieldPuns, ViewPatterns #-}
{-
- nicotool
- Copyright (C) 2020, HATTORI, Hiroki
- アラート関係
-
- Released under FreeBSD Licence.
-}
module Network.Service.NicoVideo.Alart (AlartStatus (..), AlartListener, watchAlart) where

import Control.Monad (when)
import Control.Monad.Identity
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Failure
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.UTF8 as U8
import Network.URL
import qualified Network.HTTP.Enumerator as NH
import Text.Parsec
import qualified Text.HTML.TagSoup as TS

import Network.Service.NicoVideo.XML
import Network.Service.NicoVideo.Chat

data AlartStatus = AlartStatus {
  alartUserID :: String,
  alartUserHash :: String,
  alartUserName :: String,
  alartUserPrefecture:: String,
  alartUserAge :: Int,
  alartUserSex :: String,
  alartUserIsPremium :: Bool,
  alartCommunities :: [String],
  alartMessageServer :: MessageServer
  } deriving Show

type AlartListener = (String, String, String) -> IO ()

watchAlart :: (MonadIO m) => String -> String -> AlartListener -> IO () -> m (Maybe (ChatClient, AlartStatus))
watchAlart userid password listener closer = liftIO $ do
  -- Login
  (NH.Response cd _ bdy1) <- NH.parseUrl loginurl1 >>=
                             NH.httpLbsRedirect . NH.urlEncodedBody [(C8.pack "mail", C8.pack userid),
                                                                     (C8.pack "password", C8.pack password)]
  when (cd < 200 || cd >= 300) $ liftIO $ failure $ NH.StatusCodeException cd bdy1
  let doc1 = U8.toString bdy1
  case runIdentity $ parseTag userResponseParser () loginurl1 $ TS.parseTags $ tail $ dropWhile (/= '\n') doc1 of
    Left err -> liftIO (print doc1 >> print err >> return Nothing)
    Right loginticket ->
      let loginurl2 = "http://live.nicovideo.jp/api/getalertstatus?ticket=" ++ encString False ok_url loginticket
      in do
        doc2 <- NH.simpleHttp loginurl2 >>= return . U8.toString
        case runIdentity $ parseTag alartStatusParser () loginurl2 $ TS.parseTags $ tail $ dropWhile (/= '\n') doc2 of
          Left err -> print doc2 >> print err >> return Nothing
          Right sts@(AlartStatus {..}) -> do
            client <- watchChat alartMessageServer 0
              (\_ (Chat { chatMessage }) ->
                let (liveid, rest) = break (== ',') chatMessage
                    (comid, tail -> ownerid) = break (== ',') $ tail rest
                in  if rest /= "" && elem comid alartCommunities then listener (liveid, comid, ownerid) else return ())
              (const closer)
            return $ Just (client, sts)
  where
    loginurl1 = "https://secure.nicovideo.jp/secure/login?site=nicolive_antenna"
    userResponseParser :: (Monad m) => TagParserT String u m String
    userResponseParser = element "nicovideo_user_response"
                         (\ot -> if TS.fromAttrib "status" ot == "ok"
                                 then element "ticket" txt'
                                 else fail "Login failed")
    alartStatusParser :: (Monad m) => TagParserT String u m AlartStatus
    alartStatusParser = element "getalertstatus"
                        (\_ -> do {
                            alartUserID <- element "user_id" txt';
                            alartUserHash <- element "user_hash" txt';
                            alartUserName <- element "user_name" txt';
                            alartUserPrefecture <- element "user_prefecture" txt';
                            alartUserAge <- readElement "user_age";
                            alartUserSex <- element "user_sex" txt';
                            alartUserIsPremium <- boolElement "is_premium";
                            alartCommunities <- element "communities" (\_ -> many $ element "community_id" txt');
                            alartMessageServer <- messageServerParser;
                            return $ AlartStatus {..}
                            })

