#! /usr/bin/runhaskell -threaded
{-# LANGUAGE RecordWildCards #-}

import Control.Concurrent.MVar (newMVar, modifyMVar_)
import Control.Concurrent (forkIO, yield)
import Control.Exception (bracket)
import qualified System.Glib.Types
import qualified Graphics.UI.Gtk as G
import System.Time (diffClockTimes, normalizeTimeDiff, TimeDiff (..))
import System.IO (openFile, hClose, IOMode (ReadMode) )
import System.IO.UTF8 (hGetContents)
import System.Directory (getAppUserDataDirectory)
import Text.Printf (printf)

import qualified Network.Service.NicoVideo.Alart as AL
import qualified Network.Service.NicoVideo.Chat as CH
import qualified Network.Service.NicoVideo as NV

import qualified Utils.Browser.GoogleChrome

main :: IO ()
main = do pref <- loadPreference
          --
          --
          --
          G.initGUI
          G.timeoutAddFull (yield >> return True) G.priorityDefaultIdle 100 -- これが無いとフォーカスが無い時に裏スレッドが止まる

          builder <- G.builderNew
          G.builderAddFromFile builder "nicotool.GtkBuilder"
          let getW :: (System.Glib.Types.GObjectClass b) => (G.GObject -> b) -> String -> IO b
              getW = G.builderGetObject builder
              bindAction x y = getW G.castToAction x >>= flip G.onActionActivate y
          mainwindow <- getW G.castToWindow "MainWindow"
          G.onDestroy mainwindow G.mainQuit
          bindAction "quitApplication" $ G.widgetDestroy mainwindow
          bindAction "showAbout" $ G.builderGetObject builder G.castToAboutDialog "AboutDialog" >>= G.dialogRun >> return ()

          let setupClm store name f = do col <- getW G.castToTreeViewColumn name
                                         ren <- G.cellRendererTextNew
                                         G.treeViewColumnPackStart col ren True
                                         G.cellLayoutSetAttributes col ren store $ \x -> [ G.cellText G.:= f x ]

          --
          --
          --
          chatView <- getW G.castToTreeView "ChatLogView"
          chatStore <- G.listStoreNew []
          G.treeViewSetModel chatView chatStore
          setupClm chatStore "ChatNo" $ show . CH.chatResNo . snd
          setupClm chatStore "ChatVPos" $ \(x,y) -> let TimeDiff {..} = normalizeTimeDiff $ diffClockTimes (CH.chatDate y)  x in printf "%02d:%02d" tdMin tdSec
          setupClm chatStore "ChatUserID" $ CH.chatUserID . snd
          setupClm chatStore "ChatMessage" $ CH.chatMessage . snd
          setupClm chatStore "ChatPremium" $ show . CH.chatIsPremium . snd
          setupClm chatStore "ChatMail" $ CH.chatMail . snd


          currStreamVar <- newMVar Nothing
          imgCommunity <- getW G.castToImage "imgCommunity"
          lblStreamInfo <- getW G.castToLabel "lblStreamInfo"
          let closeStream = modifyMVar_ currStreamVar $ \x -> maybe (return ()) CH.stopChat x >> return Nothing
          bindAction "closeStream" closeStream
          bindAction "openStream" $ do
            dlg <- getW G.castToDialog "dlgOpenStream"
            flip G.onClicked (G.dialogResponse dlg G.ResponseOk) =<< getW G.castToButton "btnOKToOpenStream"
            flip G.onClicked (G.dialogResponse dlg G.ResponseCancel) =<< getW G.castToButton "btnCancelToOpenStream"
            G.onDestroy dlg $ G.dialogResponse dlg G.ResponseCancel
            txtStreamID <- getW G.castToEntry "txtStreamID"
            res <- G.dialogRun dlg
            G.widgetHide dlg
            if res /= G.ResponseOk
              then return ()
              else do streamID <- G.entryGetText txtStreamID
                      ps' <- NV.getPlayerStatus streamID cookieLoader -- 動画の情報を得る
                      case ps' of
                        Left err -> print err
                        Right (NV.PlayerStatus {..}) ->  -- サーバ情報が取れたら接続
                          let basetime = NV.liveStartTime $ playerStatusLiveStream
                          in modifyMVar_ currStreamVar $ \x -> do maybe (return ()) CH.stopChat x
                                                                  G.listStoreClear chatStore
                                                                  client <- CH.watchChat playerStatusMessageServer
                                                                            defaultChatLogNum
                                                                            (\_ msg -> appendLog chatView chatStore (basetime, msg) )
                                                                            (const (return ()))
                                                                  --
                                                                  G.set lblStreamInfo [ G.labelLabel G.:= ("<span font_size='x-large'>" ++ NV.liveTitle playerStatusLiveStream) ++ "</span>\n\n" ++ (NV.liveDescription playerStatusLiveStream)]
                                                                  --
                                                                  return $ Just client



          --
          --
          --
          alartView <- getW G.castToTreeView "AlartLogView"
          alartStore <- G.listStoreNew []
          G.treeViewSetModel alartView alartStore
          setupClm alartStore "AlartTitle" $ \(_,x,_,_) -> x
          setupClm alartStore "AlartCommunity" $ \(_,_,x,_) -> x
          setupClm alartStore "AlartOwner" $ \(_,_,_,x) -> x
          (Just (alartClient, _)) <- AL.watchAlart (prefLoginID pref) (prefPassword pref)
                                     (\(liveid, comid, ownerid) -> forkIO (do si' <- NV.getStreamInfo $ "lv" ++ liveid
                                                                              case si' of
                                                                                Left err -> print err
                                                                                Right (NV.StreamInfo {..}, comname, thumbnail, ads) ->
                                                                                  appendLog alartView alartStore (streamLiveID, streamTitle, comname, ownerid)
                                                                          ) >> return () )
                                     (return ())

          G.widgetShowAll mainwindow
          G.mainGUI
          CH.stopChat alartClient
          closeStream
  where
    defaultChatLogNum = 200
    cookieLoader = Utils.Browser.GoogleChrome.cookieLoader
    appendLog :: G.TreeView -> G.ListStore a -> a -> IO ()
    appendLog view model x = do
      n <- G.listStoreAppend model x
      Just col <- G.treeViewGetColumn view 0
      G.treeViewScrollToCell view [n] col Nothing

data Preference =
  Preference {
    prefLoginID :: String,
    prefPassword :: String
    } deriving (Show, Read)


loadPreference :: IO Preference
loadPreference = do dir <- getAppUserDataDirectory "nicotool"
                    h <- openFile (dir ++ "/preference") ReadMode
                    hGetContents h >>= return . read

