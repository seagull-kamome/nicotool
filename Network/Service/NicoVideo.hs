{-# LANGUAGE ViewPatterns, PackageImports, RecordWildCards #-}
module Network.Service.NicoVideo 
       (ThumbInfo (..), Tag (..), getThumbInfo,
        communityLargeThumbnailURL, communitySmallThumbnailURL
       ) where

import Control.Monad (when)
import Control.Monad.Identity
import Control.Failure
import Data.List (dropWhile)
import Data.Maybe (fromJust, isJust, listToMaybe)
import System.Time
--import Control.Exception
--import Control.Monad (when)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.UTF8 as U8
import "utf8-string" Data.String.UTF8
import qualified Text.HTML.TagSoup as TS
import Text.Parsec
import Text.StringLike
import qualified Network.HTTP.Enumerator as NH
import Network.URL


import Utils.Browser
import Utils.Browser.GoogleChrome


--
--
--
sendRq :: String -> CookieLoader IO ->  IO [TS.Tag String]
sendRq url loader = do
  rq <- NH.parseUrl url 
  cookies <- loader $ C8.unpack $ NH.host rq
  let cookie_hdr = concatMap (\(x,y) -> encString False ok_url x ++ "=" ++ encString False ok_url y ++ ";") cookies
  (NH.Response cd _ bdy) <- NH.httpLbsRedirect
                            (rq { NH.requestHeaders = [ (C8.pack "Cookie", C8.pack cookie_hdr) ]})
  when (cd < 200 || cd >= 300) $ failure $ NH.StatusCodeException cd bdy
  return $ TS.parseTags $ tail $ dropWhile (/= '\n') $ U8.toString bdy



getTextByTagName :: String -> [TS.Tag String] -> String
getTextByTagName tagname tags = TS.fromTagText $ dropWhile (not . TS.isTagOpenName tagname) tags !! 1


--
--
--
type TagParserT str u m = ParsecT [TS.Tag str] u m

tagEater :: (Show str, StringLike str, Monad m) => (TS.Tag str -> Maybe a) -> TagParserT str u m a
tagEater = tokenPrim show (\x _ _ -> setSourceLine x (sourceLine x + 1))

tagOpen :: (Show str, StringLike str, Monad m) => str -> TagParserT str u m (TS.Tag str)
tagOpen name = tagEater $ (\x -> if TS.isTagOpenName name x then Just x else Nothing)

tagClose :: (Show str, StringLike str, Monad m) => str -> TagParserT str u m ()
tagClose name = tagEater $ (\x -> if TS.isTagCloseName name x then Just () else Nothing)

txt :: (Show str, StringLike str, Monad m) => TagParserT str u m str
txt = tagEater $ (\x -> if TS.isTagText x then Just (TS.fromTagText x) else Nothing)
txt' _ = txt

element name bdy = do { skipMany txt; ot <- tagOpen name; x <- bdy ot; skipMany txt; tagClose name; return x }

parseTag :: (Show str, StringLike str, Monad m) => TagParserT str u m a -> u -> SourceName -> [TS.Tag str] -> m (Either RequestError a)
parseTag p st name doc = runPT p st name doc >>= return . (either (Left . RequestError "Parser error" . show ) Right)


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
getThumbInfo videoID loader = do
  doc <- sendRq  ("http://ext.nicovideo.jp/api/getthumbinfo/" ++ (encString False ok_url videoID)) loader
  print doc
  return $ runIdentity $ parseTag thumbResponseParser () "getthumbinfo" doc

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



test = print $ runIdentity (parseTag thumbResponseParser () "test" doc)
  where
    doc = TS.parseTags "<nicovideo_thumb_response status=\"ok\">\n<thumb>\n<video_id>nm10937008</video_id>\n<title>\12304\20474\12398\22969\12364\12371\12435\12394\12395\21487\24859\12356\12431\12369\12364\12394\12356\12486\12540\12510\26354\12305\12354\12420\12379\12398\12486\12540\12510\12387\12413\12356\20309\12363</title>\n<description>\12495\12531\12489\12523\21517\65306DJ souchou\23550\35937\27005\26354\65306\12354\12420\12379\22909\12365\12394\12521\12494\12505\12364\12450\12491\12513\21270\12377\12427\12392\32862\12356\12390\24540\21215\12375\12414\12375\12383\12290\27468\35422\12399&hellip;\24605\12356\12388\12363\12394\12363\12387\12383\12435\12391\12377\12364\12289\12393\12358\12375\12414\12375\12423\12358\12363\12397\65311\12510\12452\12522\12473\65306mylist/15853453</description>\n<thumbnail_url>http://tn-skr1.smilevideo.jp/smile?i=10937008</thumbnail_url>\n<first_retrieve>2010-06-03T15:22:46+09:00</first_retrieve>\n<length>1:26</length>\n<movie_type>swf</movie_type>\n<size_high>2136635</size_high>\n<size_low>2136565</size_low>\n<view_counter>1732</view_counter>\n<comment_num>19</comment_num>\n<mylist_counter>8</mylist_counter>\n<last_res_body>BGM\65403\65394\65402\65392\12454\12455\12541(\65439&forall;\65377)\65417\65395 \12361\65311 \12371\12428\12399\12393\12385\12425\12391\12418\12356\12369 \20804\36020\12460\12531... </last_res_body>\n<watch_url>http://www.nicovideo.jp/watch/nm10937008</watch_url>\n<thumb_type>video</thumb_type>\n<embeddable>1</embeddable>\n<no_live_play>0</no_live_play>\n<tags domain=\"jp\">\n<tag lock=\"1\">\20474\12398\22969\12364\12371\12435\12394\12395\21487\24859\12356\12431\12369\12364\12394\12356\12486\12540\12510\26354</tag>\n<tag>\38899\27005</tag>\n<tag>\20462\27491\29256nm10970847</tag>\n</tags>\n<user_id>16824755</user_id>\n</thumb>\n</nicovideo_thumb_response>"
--
--
--
communityLargeThumbnailURL :: String -> String
communityLargeThumbnailURL x = "http://icon.nicovideo.jp/community/" ++ x ++ ".jpg"

communitySmallThumbnailURL :: String -> String
communitySmallThumbnailURL x = "http://icon.nicovideo.jp/community/s/" ++ x ++ ".jpg"


--
--
--
data MessageServer = MessageServer {
  msAddress :: String,
  msPort :: Int,
  msThread :: String
  } deriving (Show)
data LiveScreen = MainScreen | SubScreen deriving(Show)
data LiveContents = LiveContents {
  livecontentsCommand :: String,
  livecontentsScreen :: LiveScreen ,
  livecontentsStartTime :: ClockTime,
  livecontentsDuration :: Int,
  livecontentsTitle :: String
  } deriving (Show)
data LiveStream = LiveStream {
  liveTime :: ClockTime,
  liveID :: String,
  liveDefaultCommunity :: String,
  liveBaseTime :: ClockTime,
  liveOpenTime :: ClockTime,
  liveWatchCount :: Int,
  liveCommentCount :: Int,
  liveMessageServer :: MessageServer,
  liveContents :: [LiveContents]
  } deriving (Show)
             
{-
getPlayerStatus :: String -> CookieLoader IO -> IO (Maybe LiveStream)
getPlayerStatus liveid loader = do
  xml <- sendRq uri loader
  let (Document _ _ root _) = xmlParse path $ dropWhile ((/=) '<') xml
  return $ listToMaybe $ map parseLiveStream $ (deep $ tag "getplayerstatus" `with` attrval ("status", AttValue [Left "ok"])) $ CElem root noPos
  where
    parseLiveStream x =
      let st = head $ keep /> tag "stream" $ x
      in LiveStream {      
        liveTime = TOD (read $ fromJust $ getAttribute "time" x) 0,
        liveID = getElementText "id" st,
        liveDefaultCommunity = getElementText "default_community" st,
        liveBaseTime = TOD (read $ getElementText "base_time" st) 0,
        liveOpenTime = TOD (read $ getElementText "open_time" st) 0,
        liveWatchCount = read $ getElementText "watch_count" st,
        liveCommentCount = read $ getElementText "comment_count" st,
        liveMessageServer = parseMessageServer $ head $ keep /> tag "ms" $ x,
        liveContents = map parseContents $ keep /> tag "contents_list" /> tag "contents" $ x
        }
    parseMessageServer x = MessageServer {
      msAddress = getElementText "addr" x,
      msPort = read $ getElementText "port" x,
      msThread = getElementText "thread" x
      }
    parseContents x = LiveContents {
      livecontentsCommand = tagTextContent x,
      livecontentsScreen = case fromJust $ getAttribute "id" x of { "main" -> MainScreen; "sub" -> SubScreen },
      livecontentsStartTime = TOD (read $ fromJust $ getAttribute "start_time" x) 0,
      livecontentsDuration = read $ fromJust $ getAttribute "duration" x,
      livecontentsTitle = fromJust $ getAttribute "title" x
      }
    path = "http://live.nicovideo.jp/api/getplayerstatus?v=" ++ (escapeURIString isAllowedInURI liveid)
    (Just uri) = parseURI path


-}
