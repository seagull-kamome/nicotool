{-# LANGUAGE RecordWildCards, NamedFieldPuns, ViewPatterns #-}
module Preference (
  Preferences (..),
  loadPreference) where

import Control.Exception (bracket)
import System.Directory (getAppUserDataDirectory)
import qualified System.IO.UTF8 as U8 (hGetLine)
import System.IO (openFile,IOMode (..), hClose)


data Preferences =
  Preferences {
    prefUserID :: String,
    prefPassword :: String
    }

loadPreference :: IO Preferences
loadPreference = do
  dir <- getAppUserDataDirectory "nicotool"
  bracket 
    (openFile (dir ++ "/preference") ReadMode)
    hClose
    (\h -> do
        prefUserID <- U8.hGetLine h
        prefPassword <- U8.hGetLine h
        return $ Preferences {..}
        )
