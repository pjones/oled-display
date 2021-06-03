{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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
module Main (main) where

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (async, waitAnyCatchCancel)
import Control.Exception.Safe (throwTo)
import Control.Lens (makeLenses, (^.))
import Display.Arduino (Arduino)
import qualified Display.Arduino as Arduino
import qualified Display.DBus as DBus
import Display.HTTP (HTTP)
import qualified Display.HTTP as HTTP
import qualified Options.Applicative as Options
import System.Exit (ExitCode (..))
import System.Signal

-- | Command line options.
data Flags = Flags
  { flagsSerialPort :: Maybe FilePath,
    flagsDisableArduino :: Bool,
    flagsHttpSocket :: Maybe FilePath
  }

-- | Parse command line options.
flags :: Options.Parser Flags
flags =
  Flags
    <$> optional
      ( Options.strOption
          ( mconcat
              [ Options.long "serial-port",
                Options.short 'a',
                Options.metavar "FILE",
                Options.help "Use FILE as the Arduino serail port"
              ]
          )
      )
    <*> Options.switch
      ( mconcat
          [ Options.long "no-arduino",
            Options.short 'A',
            Options.help "Don't try to connect to an Arduino"
          ]
      )
    <*> optional
      ( Options.strOption
          ( mconcat
              [ Options.long "http-socket",
                Options.short 's',
                Options.metavar "FILE",
                Options.help "Use FILE for the Unix Domain Socket path"
              ]
          )
      )

-- | Run-time environment.
data Env = Env
  { -- | Arduino state.
    _envArduino :: Maybe Arduino,
    -- | Web server state.
    _envHTTP :: HTTP
  }

makeLenses ''Env

-- | Off to the races!
main :: IO ()
main = do
  Flags {..} <- Options.execParser opts

  tid <- myThreadId
  installHandler sigTERM (const $ throwTo tid ExitSuccess)

  env <-
    Env
      <$> bool
        (Just <$> Arduino.new flagsSerialPort)
        (pure Nothing)
        flagsDisableArduino
      <*> HTTP.new flagsHttpSocket

  let onMessage m = do
        case env ^. envArduino of
          Nothing -> pure ()
          Just a -> runReaderT (Arduino.message m) a
        runReaderT (HTTP.message m) (env ^. envHTTP)

  display <- case env ^. envArduino of
    Nothing -> pure []
    Just a -> one <$> async (runReaderT Arduino.run a)

  warp <- async (runReaderT HTTP.run (env ^. envHTTP))
  dbus <- async (DBus.run onMessage)

  (_, r) <- waitAnyCatchCancel (display ++ [warp, dbus])

  case r of
    Left e -> die (show e)
    Right () -> exitSuccess
  where
    opts = Options.info (Options.helper <*> flags) mempty
