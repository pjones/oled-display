{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|

Copyright:
  This file is part of the package oled-display. It is subject to the
  license terms in the LICENSE file found in the top-level directory
  of this distribution and at:

    https://code.devalot.com/pjones/oled-display.git

  No part of this package, including this file, may be copied,
  modified, propagated, or distributed except according to the terms
  contained in the LICENSE file.

License: BSD-2-Clause

-}
module Main (main) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (async, waitAnyCatchCancel)
import Control.Exception.Safe (throwTo)
import Control.Lens.TH (makeLenses)
import Control.Monad.Reader (runReaderT)
import System.Exit (ExitCode(..), die, exitSuccess)
import System.Signal

--------------------------------------------------------------------------------
-- Project Imports:
import Display.Arduino (Arduino)
import qualified Display.Arduino as Arduino
import qualified Display.DBus as DBus
import Display.HTTP (HTTP)
import qualified Display.HTTP as HTTP

--------------------------------------------------------------------------------
-- | Run-time environment.
data Env = Env
  { _envArduino :: Arduino -- ^ Arduino state.
  , _envHTTP    :: HTTP    -- ^ Web server state.
  }

makeLenses ''Env

instance Arduino.HasArduino Env where
  arduino = envArduino

instance HTTP.HasHTTP Env where
  hTTP = envHTTP

--------------------------------------------------------------------------------
-- | Off to the races!
main :: IO ()
main = do
  tid <- myThreadId
  installHandler sigTERM (const $ throwTo tid ExitSuccess)

  env <- Env <$> Arduino.new
             <*> HTTP.new

  let onMessage m = flip runReaderT env $ do
        Arduino.message m
        HTTP.message m

  display <- async (runReaderT Arduino.run env)
  warp    <- async (runReaderT HTTP.run env)
  dbus    <- async (DBus.run onMessage)

  (_, r) <- waitAnyCatchCancel [display, warp, dbus]

  case r of
    Left e   -> die (show e)
    Right () -> exitSuccess
