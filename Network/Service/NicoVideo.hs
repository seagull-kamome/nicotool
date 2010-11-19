{-# LANGUAGE ViewPatterns, PackageImports, RecordWildCards #-}
module Network.Service.NicoVideo 
       (ThumbInfo (..), Tag (..), getThumbInfo , 
        PlayerStatus (..), MessageServer (..), LiveScreen (..), LiveContents (..), 
        Que (..), UserTwitterInfo (..), LiveUser (..), LiveTelop (..), LiveStream (..), RTMP(..),
        LiveTwitter (..),
        getPlayerStatus,
        communityLargeThumbnailURL, communitySmallThumbnailURL
       ) where

import Control.Monad.Trans
import Control.Monad.Identity
import Control.Failure
import System.Time
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.UTF8 as U8
import qualified Text.HTML.TagSoup as TS
import Text.Parsec
import Text.StringLike
import qualified Network.HTTP.Enumerator as NH
import Network.URL
import qualified Network.Socket as N (PortNumber (..))

import Utils.Browser
import Utils.Browser.GoogleChrome


--
-- HTTPリクエスト用ヘルパ
--
sendRq :: (MonadIO m) => String -> CookieLoader m ->  m (U8.ByteString)
sendRq url loader = do
  rq <- liftIO $ NH.parseUrl url
  cookies <- loader $ C8.unpack $ NH.host rq
  let cookie_hdr = concatMap (\(x,y) -> encString False ok_url x ++ "=" ++ encString False ok_url y ++ ";") cookies
  (NH.Response cd _ bdy) <- liftIO $ NH.httpLbsRedirect
                            (rq { NH.requestHeaders = [ (C8.pack "Cookie", C8.pack cookie_hdr) ]})
  when (cd < 200 || cd >= 300) $ liftIO $ failure $ NH.StatusCodeException cd bdy
  return bdy

sendRqXML :: (MonadIO m) => String -> CookieLoader m ->  TagParserT String u Identity a -> u -> m (Either RequestError a)
sendRqXML url loader parser st = do
  doc <- sendRq url loader >>= return . U8.toString
  let res = runIdentity $ parseTag parser st url $ TS.parseTags $ tail $ dropWhile (/= '\n') doc
  case res of
    Left err -> liftIO (print doc >> print err)
    _ -> return ()
  return res;




--
-- XML パース用ヘルパ
--
type TagParserT str u m = ParsecT [TS.Tag str] u m

tagEater :: (Show str, StringLike str, Monad m) => (TS.Tag str -> Maybe a) -> TagParserT str u m a
tagEater = tokenPrim show (\x _ _ -> setSourceLine x (sourceLine x + 1))

tagOpen :: (Show str, StringLike str, Monad m) => str -> TagParserT str u m (TS.Tag str)
tagOpen name = tagEater $ (\x -> if TS.isTagOpenName name x then Just x else Nothing)

tagClose :: (Show str, StringLike str, Monad m) => str -> TagParserT str u m ()
tagClose name = tagEater $ (\x -> if TS.isTagCloseName name x then Just () else Nothing)

txt :: (Show str, StringLike str, Monad m) => TagParserT str u m str
txt = do { tagEater (\x -> if TS.isTagText x then Just (TS.fromTagText x) else Nothing); } <|> return (Text.StringLike.fromString "")

txt' :: (Show str, StringLike str, Monad m) => a -> TagParserT str u m str
txt' _ = txt


element :: (Show str, StringLike str, Monad m) => str -> (TS.Tag str -> TagParserT str u m a) -> TagParserT str u m a
element name bdy = do { ot <- Text.Parsec.try (skipText >> tagOpen name); x <- bdy ot; skipText; tagClose name; return x }
  where skipText = skipMany $ tagEater (\x -> if TS.isTagText x then Just x else Nothing)

parseTag :: (Show str, StringLike str, Monad m) => TagParserT str u m a -> u -> SourceName -> [TS.Tag str] -> m (Either RequestError a)
parseTag p st name doc = runPT p st name doc >>= return . (either (Left . RequestError "Parser error" . show ) Right)


readElement :: (Read b, Show str, StringLike str, Monad m) => str -> TagParserT str u m b
readElement name = element name (\_ -> txt >>= return . read . Text.StringLike.toString)

unixTimeElement :: (Show str, StringLike str, Monad m) => str -> TagParserT str u m ClockTime
unixTimeElement name = return . flip TOD 0 =<< readElement name

boolElement :: (Show str, StringLike str, Monad m) => str -> TagParserT str u m Bool
boolElement name = do { x <- element name txt'; return $ Text.StringLike.toString x == "1"; }



--
--
--
data RequestError = RequestError String String deriving(Show)


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
  msPort :: N.PortNumber,
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
    playerRTMP :: RTMP,
    playerStatusMessageServer :: MessageServer,
    playerTIDList :: [String],
    playerTwitter :: LiveTwitter
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
    playerRTMP <- element "rtmp" $ 
                  \ot -> do { x <- element "url" txt'; y <- element "ticket" txt'; return $ RTMP (TS.fromAttrib "is_fms" ot == "1") x y };
    playerStatusMessageServer <- element "ms" messageServerParser;
    playerTIDList <- element "tid_list" $ return . const [];
    playerTwitter <- element "twitter" $ \_ -> do {
      twitterLiveEnabled <- boolElement "live_enabled";
      twitterVIPModeCount <- readElement "vip_mode_count";
      twitterLiveAPIURL <- element "live_api_url" txt';
      return $ LiveTwitter {..} };
    element "player" $ \_ -> element "error_report" txt';
    let playerStatusTime = (TOD (read $ TS.fromAttrib "time" ot) 0)
    in return $ PlayerStatus  {..}
}



messageServerParser :: (Monad m) => a -> TagParserT String u m MessageServer
messageServerParser _ = do {
  msAddress <- element "addr" txt';
  portnum <- readElement "port";
  msPort <- return $ N.PortNum $ fromIntegral (portnum :: Int);
  msThread <- element "thread" txt';
  return $ MessageServer {..}
  }


--
-- 生放送コメント
--


