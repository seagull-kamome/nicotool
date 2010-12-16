{-# LANGUAGE RecordWildCards #-}
{-
- nicotool
- Copyright (C) 2020, HATTORI, Hiroki
- コメント関係
-
- Released under FreeBSD Licence.
-}
module Network.Service.NicoVideo.Chat
       (MessageServer (..), messageServerParser,
        --
        ChatClient (..), UserRegion (..), Chat (..),
        ChatListener, ChatCloser,
        watchChat, stopChat
       ) where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, modifyMVar, readMVar)
import System.Time
import Network.Service.NicoVideo.XML
import qualified Network.Socket as N
import qualified Network.XMLSocket as XS
import qualified Text.HTML.TagSoup as TS


data MessageServer = MessageServer {
  msAddress :: String,
  msPort :: String,
  msThread :: String
  } deriving (Show)





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
newtype ChatClient = ChatClient XS.XMLSocket
data UserRegion = FreeRider | Premium | Alart | MovieOwner | SysOp | BSP Int| Unknown String deriving (Show, Eq)
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


--
--
--
watchChat :: (MonadIO m) => MessageServer -> Int -> ChatListener -> ChatCloser -> m ChatClient
watchChat (MessageServer {..})  lognum listener closer = liftIO $ do
  state <- newEmptyMVar
  (addr:_) <- N.getAddrInfo Nothing (Just msAddress) (Just msPort)
  sock <- XS.startClient addr (onReceive state) (onClose state)
  putMVar state ("", 0 :: Int, ChatClient sock)
  XS.send sock [TS.TagOpen "thread" [("thread", msThread), ("res_from", show $ negate lognum), ("version", "20061206")], TS.TagClose "thread" ]
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
          "" -> FreeRider; "1" -> Premium; "2" -> Alart; "3" -> MovieOwner; "4" -> SysOp; "6" -> BSP 1; "7" -> BSP 2; x -> Unknown x }
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

stopChat :: ChatClient -> IO ()
stopChat (ChatClient sock) = XS.closeSession sock >> return ()
