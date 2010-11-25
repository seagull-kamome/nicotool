{-# LANGUAGE ViewPatterns, PackageImports, RecordWildCards, NamedFieldPuns #-}
module Network.Service.NicoVideo 
       (ThumbInfo (..), Tag (..), getThumbInfo , 
        PlayerStatus (..), MessageServer (..), LiveScreen (..), LiveContents (..), 
        Que (..), UserTwitterInfo (..), LiveUser (..), LiveTelop (..), LiveStream (..), RTMP(..),
        LiveTwitter (..),
        getPlayerStatus,
        communityLargeThumbnailURL, communitySmallThumbnailURL,
        --
        ChatClient, Chat (..), watchChat,
        --
        AlartStatus (..), AlartListener, watchAlart,
        StreamInfo (..), getStreamInfo
       ) where

import Control.Monad.Trans
import Control.Monad.Identity
import Control.Concurrent.MVar (newEmptyMVar, putMVar, modifyMVar, readMVar)
import Control.Failure
import System.Time
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.UTF8 as U8
import qualified Text.HTML.TagSoup as TS
import Text.Parsec
import qualified Network.HTTP.Enumerator as NH
import Network.URL
import qualified Network.Socket as N
import qualified Network.XMLSocket as XS

import Utils.Browser
import Utils.Browser.GoogleChrome

import Network.Service.NicoVideo.XML
import Network.Service.NicoVideo.HTTP


--
--
--
data Tag = Tag { tagCategory :: Bool, tagLock :: Bool, tagName :: String } deriving (Show)
data ThumbInfo = 
  ThumbInfo {
    thumbVideoID :: String,
    thumbTitle :: String,
    thumbDescription :: String,
    thumbThumbnailURL :: String,
    thumbFirstRetrieve :: String,
    thumbLength :: String,
    thumbMovieType :: String,
    thumbSizeHigh :: Int,
    thumbSizeLow :: Int,
    thumbViewCounter :: Int,
    thumbCommentNum :: Int,
    thumbMyListNum :: Int,
    thumbLastResBody :: String,
    thumbWatchURL :: String,
    thumbThumbType :: String,
    thumbEmbeddable :: Bool,
    thumbLivePlay :: Bool,
    thumbTags :: [(String, [Tag])],
    thumbUserID :: String
    }
  deriving (Show)


getThumbInfo :: String -> CookieLoader IO -> IO (Either RequestError ThumbInfo)
getThumbInfo videoID loader = sendRqXML url loader thumbResponseParser ()
  where url = "http://ext.nicovideo.jp/api/getthumbinfo/" ++ (encString False ok_url videoID)


thumbResponseParser :: (Monad m) => TagParserT String u m ThumbInfo
thumbResponseParser = element "nicovideo_thumb_response" (\_ -> thumbParser)

thumbParser :: (Monad m) => TagParserT String u m ThumbInfo
thumbParser = element "thumb" $ \_ -> do {
  thumbVideoID <- element "video_id" txt';
  thumbTitle <- element "title" txt';
  thumbDescription <- element "description" txt';
  thumbThumbnailURL <- element "thumbnail_url" txt';
  thumbFirstRetrieve <- element "first_retrieve" txt';
  thumbLength <- element "length" txt';
  thumbMovieType <- element "movie_type" txt';
  (read -> thumbSizeHigh) <- element "size_high" txt';
  (read -> thumbSizeLow) <- element "size_low" txt';
  (read -> thumbViewCounter) <-  element "view_counter" txt';
  (read -> thumbCommentNum) <- element "comment_num" txt';
  (read ->thumbMyListNum) <- element "mylist_counter" txt';
  thumbLastResBody <- element "last_res_body" txt';
  thumbWatchURL <- element "watch_url" txt';
  thumbThumbType <- element "thumb_type" txt';
  ((== "1") -> thumbEmbeddable) <- element "embeddable" txt';
  ((== "0") -> thumbLivePlay) <- element "no_live_play" txt';
  thumbTags <- many $ tagsParser;
  thumbUserID <- element "user_id" txt';
  return $ ThumbInfo {..}
  }

tagsParser :: (Monad m) => TagParserT String u m (String, [Tag])
tagsParser = element "tags" $ \opentag -> do {
  tags <- many $ element "tag" (\x -> txt >>= return . Tag (TS.fromAttrib "category" x /= "") (TS.fromAttrib "lock" x /= ""));
  return (TS.fromAttrib "domain" opentag, tags)
  }



--
-- コミュニティアイコン
--
communityLargeThumbnailURL :: String -> String
communityLargeThumbnailURL x = "http://icon.nicovideo.jp/community/" ++ x ++ ".jpg"

communitySmallThumbnailURL :: String -> String
communitySmallThumbnailURL x = "http://icon.nicovideo.jp/community/s/" ++ x ++ ".jpg"


--
-- 生放送
--
data MessageServer = MessageServer {
  msAddress :: String,
  msPort :: String,
  msThread :: String
  } deriving (Show)
data LiveScreen = MainScreen | SubScreen deriving(Show)
data LiveContents = LiveContents {
  livecontentsCommand :: String,
  livecontentsScreen :: LiveScreen ,
  livecontentsStartTime :: ClockTime,
  livecontentsDisableVideo :: Bool,
  livecontentsDisableAudio :: Bool
  } deriving (Show)
data Que = Que { queVPos :: Int, queMail :: String, queName :: String, queURL :: String } deriving (Show)
data UserTwitterInfo = TwitterInfo {
  twitterStatus :: String,
  twitterAfterAuth :: Bool,
  twitterScreenName :: String,
  twitterFollowersCount :: Int,
  twitterIsVIP :: Bool,
  twitterProfileImageURL :: String,
  twitterToken :: String
  } deriving (Show)
data LiveUser = LiveUser {
  liveuserRoomLabel :: String,
  liveuserRoomSeetNo :: String,
  liveuserAge :: Int,
  liveuserSex :: Int,
  liveuserPrefecture :: Int,
  liveuserNick :: String,
  liveuserIsPremium :: Bool,
  liveuserID :: String,
  liveuserIsJoin :: Bool,
  liveuserIMMUComment :: Int,
  liveuserCanBroadcast :: Bool,
  liveuserCanForceLogin :: Bool,
  liveuserTwitterInfo :: UserTwitterInfo
  } deriving (Show)
data LiveTelop = LiveTelop Bool deriving Show
data LiveStream = LiveStream {
  liveID :: String,
  liveWatchCount :: Int,
  liveTitle :: String,
  liveDescription :: String,
  liveCommentCount :: Int,
  liveDanjoCommentMode :: Bool,
  liveHQStream :: Bool,
  liveRelayComment :: Bool,
  livePark :: Bool,
  liveBourbonURL :: String,
  liveFullVideo :: String,
  liveAfterVideo :: String,
  liveBeforeVideo :: String,
  liveKickoutVideo :: String,
  liveHeaderComment :: Bool,
  liveFooterComment :: Bool,
  livePluginDelay :: String,
  livePluginURL :: String,
  livePluginURLs :: String,
  liveProviderType :: String,
  liveDefaultCommunity :: String,
  liveArchive :: Bool,
  liveIsDJStream :: Bool,
  liveTwitterTag :: String,
  liveIsOwner :: Bool,
  liveOwnerID :: String,
  liveIsReserved :: Bool,
  liveBaseTime :: ClockTime,
  liveOpenTime :: ClockTime,
  liveStartTime :: ClockTime,
  liveEndTime :: Maybe ClockTime,
  liveTimeShiftTime :: Maybe ClockTime,
  liveQue :: [Que],
  liveTelop :: Maybe LiveTelop,
  liveIchibaNoticeEnable :: Maybe Bool,
  liveCommentLock :: Maybe Bool,
  liveBackgroundComment :: Maybe Bool,
  liveContents :: [LiveContents],
  livePress :: (Int, Int, String)
  } deriving (Show)
data RTMP = RTMP { rtmpIsFMS :: Bool, rtmpURL :: String, rtmpTicket :: String } deriving Show
data LiveTwitter = LiveTwitter { twitterLiveEnabled :: Bool, twitterVIPModeCount :: Int, twitterLiveAPIURL :: String } deriving Show
data PlayerStatus = 
  PlayerStatus {
    playerStatusTime :: ClockTime,
    playerStatusLiveStream :: LiveStream,
    playerStatusUser :: LiveUser,
    playerStatusRTMP :: RTMP,
    playerStatusMessageServer :: MessageServer,
    playerStatusTIDList :: [String],
    playerStatusTwitter :: LiveTwitter
    } 
  | PlayerUnknownError
  | PlayerComingsoon
  deriving Show

getPlayerStatus :: String -> CookieLoader IO -> IO (Either RequestError PlayerStatus)
getPlayerStatus liveid loader = sendRqXML url loader playerStatusParser ()
  where url = "http://live.nicovideo.jp/api/getplayerstatus/" ++ (encString False ok_url liveid)

playerStatusParser :: (Monad m) => TagParserT String u m PlayerStatus
playerStatusParser = 
  element "getplayerstatus" $ \ot -> do {
    playerStatusLiveStream <- element "stream" $ \_ -> do {
       liveID <- element "id" txt';
       liveWatchCount <- readElement "watch_count";
       liveTitle <- element "title" txt';
       liveDescription <- element "description" txt';
       liveCommentCount <- readElement "comment_count";
       liveDanjoCommentMode <- boolElement "danjo_comment_mode";
       liveHQStream <- boolElement "hqstream";                        
       liveRelayComment <- boolElement "relay_comment";
       livePark <- boolElement "park";
       liveBourbonURL <- element "bourbon_url" txt';
       liveFullVideo <- element "full_video" txt';
       liveAfterVideo <- element "after_video" txt';
       liveBeforeVideo <- element "before_video" txt';
       liveKickoutVideo <- element "kickout_video" txt';
       liveHeaderComment <- boolElement "header_comment";
       liveFooterComment <- boolElement "footer_comment";
       livePluginDelay <- element "plugin_delay" txt';
       livePluginURL <- element "plugin_url" txt';
       livePluginURLs <- element "plugin_urls" txt';
       liveProviderType <- element "provider_type" txt';
       liveDefaultCommunity <- element "default_community" txt';
       liveArchive <- boolElement "archive";
       liveIsDJStream <- boolElement "is_dj_stream";
       liveTwitterTag <- element "twitter_tag" txt';
       liveIsOwner <- boolElement "is_owner";
       liveOwnerID <- element "owner_id" txt';
       liveIsReserved <- boolElement "is_reserved";
       liveBaseTime <- unixTimeElement "base_time";
       liveOpenTime <- unixTimeElement "open_time";
       liveStartTime <- unixTimeElement "start_time";
       -- ここから終了している枠のみ
       liveEndTime <- option Nothing $ unixTimeElement "end_time" >>= return . Just;
       liveTimeShiftTime <- option Nothing $ unixTimeElement "timeshift_time" >>= return . Just;
       liveQue <- option [] $ element "quesheet" (\_ -> many $ element "que" (\ot -> do { 
                                                                                 x <- txt; 
                                                                                 return $ Que (read $ TS.fromAttrib "vpos" ot) 
                                                                                 (TS.fromAttrib "mail" ot) (TS.fromAttrib "name" ot) x
                                                                                 }) );
       -- ここまで。そして、ここから放送中の枠のみ
       liveTelop <- option Nothing $ element "telop" $ \_ -> do { x <- boolElement "enable"; return $ LiveTelop x } >>= return . Just;
       liveIchibaNoticeEnable <- option Nothing $ boolElement "ichiba_notice_enable" >>= return . Just;
       liveCommentLock <- option Nothing $ boolElement "comment_lock" >>= return . Just;
       liveBackgroundComment <- option Nothing $ boolElement "background_comment" >>= return . Just;
       liveContents <- option [] $ element "contents_list" $ 
                       \_ -> let convID "main" = MainScreen
                                 convID "sub" = SubScreen
                                 convID _ = undefined
                             in  do {
                               many $ element "contents" (\ot -> do { x <- txt;
                                                                      return $ LiveContents x 
                                                                      (convID $ TS.fromAttrib "id" ot)
                                                                      (TOD (read $ TS.fromAttrib "start_time" ot) 0)
                                                                      (TS.fromAttrib "disableVideo" ot == "1")
                                                                      (TS.fromAttrib "disableAudio" ot == "1"); })
                               };
       -- ここまで。
       livePress <- element "press" 
                    (\_ -> do { x <- readElement "display_lines"; y <- readElement "display_time"; z <- element "style_conf" txt'; return (x,y,z) });
       return $ LiveStream {..}
       };
    playerStatusUser <- element "user" $ \_ -> do {
      liveuserRoomLabel <- element "room_label" txt';
      liveuserRoomSeetNo <- element "room_seetno" txt';
      liveuserAge <- readElement "userAge";
      liveuserSex <- readElement "userSex";
      liveuserPrefecture <- readElement "userPrefecture";
      liveuserNick <- element "nickname" txt';
      liveuserIsPremium <- boolElement "is_premium";
      liveuserID <- element "user_id" txt';
      liveuserIsJoin <- boolElement "is_join";
      liveuserIMMUComment <- readElement "immu_comment";
      liveuserCanBroadcast <- boolElement "can_broadcast";
      liveuserCanForceLogin <- boolElement "can_forcelogin";
      liveuserTwitterInfo <- element "twitter_info" $ \_ -> do {
        twitterStatus <- element "status" txt';
        twitterAfterAuth <- boolElement "after_auth";
        twitterScreenName <- element "screen_name" txt';
        twitterFollowersCount <- readElement "followers_count";
        twitterIsVIP <- boolElement "is_vip";
        twitterProfileImageURL <- element "profile_image_url" txt';
        twitterToken <- element "tweet_token" txt';
        return $ TwitterInfo {..}
        };
      return $ LiveUser {..}
      };
    playerStatusRTMP <- element "rtmp" $ 
                        \ot -> do { x <- element "url" txt'; y <- element "ticket" txt'; return $ RTMP (TS.fromAttrib "is_fms" ot == "1") x y };
    playerStatusMessageServer <- messageServerParser;
    playerStatusTIDList <- element "tid_list" $ return . const [];
    playerStatusTwitter <- element "twitter" $ \_ -> do {
      twitterLiveEnabled <- boolElement "live_enabled";
      twitterVIPModeCount <- readElement "vip_mode_count";
      twitterLiveAPIURL <- element "live_api_url" txt';
      return $ LiveTwitter {..} };
    element "player" $ \_ -> element "error_report" txt';
    let playerStatusTime = (TOD (read $ TS.fromAttrib "time" ot) 0)
    in return $ PlayerStatus  {..}
}



messageServerParser :: (Monad m) => TagParserT String u m MessageServer
messageServerParser = element "ms" (\_ -> do {
                                       msAddress <- element "addr" txt';
                                       msPort <- element "port" txt';
                                       msThread <- element "thread" txt';
                                       return $ MessageServer {..}
                                       })



--
--
--
data StreamInfo = 
  StreamInfo {
    streamLiveID :: String,
    streamTitle :: String,
    streamDescription :: String,
    streamProviderType :: String,
    streamDefaultCommunity :: String
    } deriving Show
getStreamInfo :: String -> CookieLoader IO -> IO (Either RequestError (StreamInfo, (String, String)) )
getStreamInfo liveid loader = sendRqXML url loader streamInfoParser ()
  where 
    url = "http://live.nicovideo.jp/api/getstreaminfo/lv" ++ liveid
    streamInfoParser :: (Monad m) => TagParserT String u m (StreamInfo, (String, String))
    streamInfoParser = do {
      streaminfo <- element "getstreaminfo" 
                    (\ot -> do {
                        streamLiveID <- element "request_id" txt';
                        element "streaminfo" (\_ -> do {
                                                 streamTitle <- element "title" txt';
                                                 streamDescription <- element "description" txt';
                                                 streamProviderType <- element "provider_type" txt';
                                                 streamDefaultCommunity <- element "default_community" txt';
                                                 return $ StreamInfo {..} }) });
      communityinfo <- element "communityinfo" (\_ -> do {
                                                   x <- element "name" txt';
                                                   y <- element "thumbnail" txt';
                                                   return (x,y); });
      return (streaminfo, communityinfo) }





--
-- コメント
--
newtype ChatClient = ChatClient XS.XMLSocket
data UserRegion = FreeRider | Premium | Alart | MovieOwner | SysOp | Unknown String deriving (Show, Eq)
data Chat = 
  Chat { 
    chatResNo :: Int, 
    chatDate :: ClockTime,
    chatUserID :: String, 
    chatVPos :: String, 
    chatIsPremium :: UserRegion,
    chatMail :: String, 
    chatAnonymity :: Bool,
    chatMessage :: String
    } deriving Show
type ChatListener = ChatClient -> Chat -> IO ()
type ChatCloser = ChatClient -> IO ()

watchChat :: (MonadIO m) => MessageServer -> ChatListener -> ChatCloser -> m ChatClient
watchChat (MessageServer {..})  listener closer = liftIO $ do
  state <- newEmptyMVar
  (addr:_) <- N.getAddrInfo Nothing (Just msAddress) (Just msPort)
  sock <- XS.startClient addr (onReceive state) (onClose state)
  putMVar state ("", 0 :: Int, ChatClient sock)
  XS.send sock [TS.TagOpen "thread" [("thread", msThread), ("res_from", "-200"), ("version", "20061206")], TS.TagClose "thread" ]
  return $ ChatClient sock
  where
    heartbeat = undefined
    onReceive state _ (x@(TS.TagOpen "thread" _):_ ) =
      modifyMVar state (\(ticket, last_res, client) -> return (((TS.fromAttrib "ticket" x), (read $ TS.fromAttrib "last_res" x), client), ()) )
    onReceive state sock (x@(TS.TagOpen "chat" _):TS.TagText chatMessage:_) = do
      (_,_,client) <- readMVar state
      if chatIsPremium == MovieOwner && chatMessage == "/disconnect"
        then do { XS.closeSession sock; return () }
        else listener client $ Chat {..}
      where
        chatResNo = read $ TS.fromAttrib "no" x 
        chatDate = TOD (read $ TS.fromAttrib "date" x) 0
        chatUserID = TS.fromAttrib "user_id" x
        chatVPos = TS.fromAttrib "vpos" x
        chatIsPremium = case TS.fromAttrib "premium" x of { 
          "" -> FreeRider; "1" -> Premium; "2" -> Alart; "3" -> MovieOwner; "4" -> SysOp; x -> Unknown x }
        chatMail = TS.fromAttrib "mail" x
        chatAnonymity = TS.fromAttrib "anonymity" x == "1"
    onReceive _ _ xs = do
      mapM_ putStr ["Unknwon message", TS.renderTags xs, "\n"]
    onClose state sock = do { (_,_,client) <- readMVar state; closer client; return () }
                

{-
testChat x = do  
  Right sts <- getPlayerStatus x cookieLoader
  f <- newEmptyMVar
  watchChat (playerStatusMessageServer sts) onChatReceived (\_ -> putMVar f True)
  takeMVar f
  return ()
  where
    cookieLoader = Utils.Browser.GoogleChrome.cookieLoader
    onChatReceived client (Chat {..})= do
      mapM_ putStr $ intersperse " | " [show chatResNo, show chatDate, chatUserID, chatMessage, show chatIsPremium, chatMail, "\n"]
      return ()
-}
  
--
-- Alart
--
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

watchAlart :: (MonadIO m) => String -> String -> AlartListener -> IO () -> m (Maybe AlartStatus)
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
            watchChat alartMessageServer
              (\_ (Chat { chatMessage }) ->
                let (liveid, rest) = break (== ',') chatMessage
                    (comid, tail -> ownerid) = break (== ',') $ tail rest
                in  if rest /= "" && elem comid alartCommunities then listener (liveid, comid, ownerid) else return ())
              (const closer)
            return $ Just sts
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

