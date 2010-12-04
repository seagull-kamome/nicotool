{-# LANGUAGE ViewPatterns, PackageImports, RecordWildCards, NamedFieldPuns #-}
module Network.Service.NicoVideo 
       (ThumbInfo (..), Tag (..), getThumbInfo , 
        PlayerStatus (..), LiveScreen (..), LiveContents (..), 
        Que (..), UserTwitterInfo (..), LiveUser (..), LiveTelop (..), LiveStream (..), RTMP(..),
        LiveTwitter (..),
        getPlayerStatus,
        communityLargeThumbnailURL, communitySmallThumbnailURL,
        --
        StreamInfo (..), getStreamInfo
       ) where

import System.Time
import qualified Text.HTML.TagSoup as TS
import Text.Parsec
import Network.URL

import Utils.Browser
import Utils.Browser.GoogleChrome

import Network.Service.NicoVideo.XML
import Network.Service.NicoVideo.HTTP
import Network.Service.NicoVideo.Chat

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
getThumbInfo videoID loader = sendRqXML url (Just loader) thumbResponseParser ()
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
getPlayerStatus liveid loader = sendRqXML url (Just loader) playerStatusParser ()
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
       liveEndTime <- option Nothing $ unixTimeElement "end_time" >>= return . Just;
       liveStartTime <- unixTimeElement "start_time";
       -- ここから終了している枠のみ
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
getStreamInfo :: String -> IO (Either RequestError (StreamInfo, String, String, [(String,String)]) )
getStreamInfo liveid = sendRqXML url Nothing streamInfoParser ()
  where 
    url = "http://live.nicovideo.jp/api/getstreaminfo/" ++ liveid
    streamInfoParser :: (Monad m) => TagParserT String u m (StreamInfo, String, String, [(String, String)])
    streamInfoParser =
      element "getstreaminfo" 
                    (\ot -> do {
                        streamLiveID <- element "request_id" txt';
                        streaminfo <- element "streaminfo" (\_ -> do {
                                                               streamTitle <- element "title" txt';
                                                               streamDescription <- element "description" txt';
                                                               streamProviderType <- element "provider_type" txt';
                                                               streamDefaultCommunity <- element "default_community" txt';
                                                               return $ StreamInfo {..} });
                        (comName, thumbnail) <- element "communityinfo" (\_ -> do {
                                                                            x <- element "name" txt';
                                                                            y <- element "thumbnail" txt';
                                                                            return (x,y) });
                        ads <- option [] $ do {
                          element "adsense" (\_ -> many $ element "item" $ \_ -> do {
                                                adname <- element "name" txt';
                                                adurl <- element "url" txt';
                                                return (adname, adurl)
                                                })
                          };
                        return (streaminfo, comName, thumbnail, ads)
                        })






