{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
module Main (main) where

--------------------------------------------------------------------------------
import qualified Display.DBus as DBus
import qualified Display.Arduino as Arduino
import Control.Concurrent.Async (async, wait)
import Control.Monad.Reader (runReaderT)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  arduino <- Arduino.new
  display <- async (runReaderT Arduino.run arduino)
  dbus <- async (DBus.run (\m -> runReaderT (Arduino.message m) arduino))
  wait display
  wait dbus
