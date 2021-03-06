{-# LANGUAGE OverloadedStrings #-}

-- |
--
-- Copyright:
--   This file is part of the package oled-display. It is subject to the
--   license terms in the LICENSE file found in the top-level directory
--   of this distribution and at:
--
--     https://code.devalot.com/pjones/oled-display.git
--
--   No part of this package, including this file, may be copied,
--   modified, propagated, or distributed except according to the terms
--   contained in the LICENSE file.
--
-- License: BSD-2-Clause
--
-- Receive messages from D-Bus signals.
module Display.DBus
  ( run,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import qualified DBus
import qualified DBus.Client as DBus
import qualified Data.ByteString.Char8 as ByteString
import Display.Message
import Display.Timer (pomodoro)

run :: (Message -> IO ()) -> IO ()
run handler =
  bracket DBus.connectSession DBus.disconnect go
  where
    go :: DBus.Client -> IO ()
    go client = do
      _ <- DBus.addMatch client orgClockMatch orgClockHandler
      forever (threadDelay 1000000)

    orgClockMatch :: DBus.MatchRule
    orgClockMatch =
      DBus.matchAny
        { DBus.matchInterface = Just "org.gnu.Emacs.Org.Clock"
        }

    orgClockHandler :: DBus.Signal -> IO ()
    orgClockHandler sig =
      case DBus.formatMemberName (DBus.signalMember sig) of
        "Stopped" -> handler TimerStop
        "Started" -> startMessage (DBus.signalBody sig)
        _ -> pure ()

    startMessage :: [DBus.Variant] -> IO ()
    startMessage [t, m] = do
      let timer = TimerStart . pomodoro <$> (DBus.fromVariant t :: Maybe Word32)
          msg = TimerMessage . ByteString.pack <$> DBus.fromVariant m

      maybe (pure ()) handler timer
      maybe (pure ()) handler msg
    startMessage _ = pure ()
