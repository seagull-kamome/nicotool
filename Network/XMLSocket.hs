{-# LANGUAGE RecordWildCards #-}
{-
- XML Socket module for Haskell
- Copyright 2010 (c) HATTORI,Hiroki
-
-
-}
module Network.XMLSocket
       (ReceiveHandler, CloseHandler,
        XMLSocket, XMLSocketServer,
        startClient, startServer, closeSession, stopServer, send
       ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (readMVar, newMVar, swapMVar, MVar)
import qualified Data.ByteString.Lazy.Char8 as LBS (split, empty, append, length)
import qualified Data.ByteString.Lazy.UTF8 as UTF (toString, fromString)
import qualified Network.Socket as N
import qualified Network.Socket.ByteString.Lazy as NBS
import qualified Text.HTML.TagSoup as TS


type ReceiveHandler = XMLSocket -> [TS.Tag String] -> IO ()
type CloseHandler = XMLSocket -> IO ()
data XMLSocket = XMLSocket N.Socket (MVar Bool)
data XMLSocketServer = XMLSocketServer N.Socket (MVar Bool) [XMLSocket]



startClient :: N.AddrInfo -> ReceiveHandler -> CloseHandler -> IO XMLSocket
startClient addr receiver closer = do
	s <- N.socket (N.addrFamily addr) N.Stream N.defaultProtocol
	N.connect s $ N.addrAddress addr
	newsock <- newMVar False >>= return . XMLSocket s
	forkIO $ sessionLoop newsock receiver closer
	return newsock



startServer :: N.AddrInfo -> ReceiveHandler -> CloseHandler -> IO XMLSocketServer
startServer addr receiver closer = do
	s <- N.socket (N.addrFamily addr)  N.Stream N.defaultProtocol
        N.bindSocket s $ N.addrAddress addr
	N.listen s 5
        newmv <- newMVar False
        let newsocket = XMLSocketServer s newmv []
            loop = do
              (s', _) <- N.accept s
              newmv' <- newMVar False
              forkIO $ sessionLoop (XMLSocket s' newmv') receiver closer
              loop
          in forkIO loop >> return newsocket

closeSession :: XMLSocket -> IO Bool
closeSession (XMLSocket _ x) = swapMVar x True


stopServer :: XMLSocketServer -> IO Bool
stopServer = undefined -- FIXME:

sessionLoop :: XMLSocket -> ReceiveHandler -> CloseHandler -> IO ()
sessionLoop s@(XMLSocket sock flg) receiver closer =
  let fin = closer s >> N.sClose sock
      loop buf = do
        stopRequired <- readMVar flg
        connected <- N.sIsConnected sock
        if stopRequired || not connected
          then fin
          else do
            newchunk <- NBS.recv sock 1024
            if LBS.length newchunk == 0
              then fin
              else let f (x:[]) = loop x
                       f (x:xs) = receiver s (TS.parseTags $ UTF.toString x) >> f xs
                   in f $ LBS.split '\0' (LBS.append buf newchunk)
  in loop $ LBS.empty


send :: XMLSocket -> [TS.Tag String] -> IO ()
send (XMLSocket sock _) tags =
	NBS.sendAll sock $ UTF.fromString $ TS.renderTagsOptions (TS.RenderOptions TS.escapeHTML (const True)) tags ++ " \0"