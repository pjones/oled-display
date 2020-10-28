{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
module Display.Arduino
  ( Arduino,
    HasArduino (arduino),
    new,
    run,
    message,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait, waitAnyCatchCancel)
import Control.Concurrent.STM.TQueue
import Control.Exception (bracket)
import Control.Lens (view)
import Control.Lens.TH (makeClassy)
import Control.Monad.Except (throwError)
import Control.Monad.STM.Class (MonadSTM, liftSTM)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAscii, isPrint)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Display.Message
import Display.Timer (Timer)
import qualified Display.Timer as Timer
import System.Directory (doesFileExist)
import System.Hardware.Serialport (SerialPort)
import qualified System.Hardware.Serialport as Serial

-- | Information about what the screen is displaying.
data Screen
  = -- | Not showing anything on the display.
    Blank
  | -- | Display a timer that started at the given time.
    Timer Timer
  | -- | Unknown ASCII characters.
    Chars ByteString

-- | Commands we can send to the Arduino.
data Command
  = Flash Int
  | Reset
  | TextSize Int
  | TextContent ByteString
  deriving (Show)

-- | The state for controlling an arduino.
data Arduino = Arduino
  { _screen :: TVar Screen,
    _queue :: TQueue Command,
    _serialPath :: FilePath
  }

makeClassy ''Arduino

-- | Process an incoming message.  The screen will be updated the next
-- time that 'draw' is called.
message ::
  ( MonadReader e m,
    HasArduino e,
    MonadSTM m
  ) =>
  Message ->
  m ()
message msg =
  case update msg of
    Nothing -> pure ()
    Just scr -> do
      var <- view (arduino . screen)
      liftSTM $ writeTVar var scr
  where
    update :: Message -> Maybe Screen
    update m = case m of
      TimerStart t -> Just (Timer t)
      TimerStop -> Just Blank
      TimerMessage _ -> Nothing

-- | Update the display.
draw ::
  forall e m.
  ( MonadReader e m,
    HasArduino e,
    MonadSTM m
  ) =>
  UTCTime ->
  m ()
draw now = do
  vars <- view arduino

  liftSTM $ do
    scr <- readTVar (_screen vars)

    let commands = case scr of
          Blank -> [Reset]
          Timer t -> drawTimer t now
          Chars cs -> drawChars cs

    mapM_ (writeTQueue $ _queue vars) commands

-- | Commands to draw a timer relative to the current time.
drawTimer :: Timer -> UTCTime -> [Command]
drawTimer timer now =
  drawChars formatted ++ [Flash flash]
  where
    formatted :: ByteString
    formatted = Timer.render timer now
    flash :: Int
    flash =
      case Timer.remaining timer now of
        Timer.Negative _ -> 250
        Timer.Positive n
          | n > 120 -> 0
          | otherwise -> 500

-- | Commands to draw arbitrary text.
drawChars :: ByteString -> [Command]
drawChars chars =
  [ TextSize (size chars),
    TextContent chars
  ]
  where
    size :: ByteString -> Int
    size bs
      | ByteString.length bs < 6 = 4
      | ByteString.length bs < 10 = 3
      | ByteString.length bs < 20 = 2
      | otherwise = 1

-- | Send a command to the arduino.
send :: SerialPort -> Command -> ExceptT ByteString IO ()
send port c = do
  sent <- liftIO (Serial.send port msg)

  unless (sent == ByteString.length msg) $
    throwError "failed to send entire message"
  where
    msg :: ByteString
    msg = LBS.toStrict $ Builder.toLazyByteString (chars <> "\n\n\r")
    chars :: Builder
    chars = case c of
      Flash n -> "F" <> Builder.intDec n
      Reset -> "R"
      TextSize n -> "S" <> Builder.intDec n
      TextContent bs -> "T" <> Builder.byteString (clean bs)
    clean :: ByteString -> ByteString
    clean = ByteString.map clean'
      where
        clean' char =
          if isAscii char && isPrint char
            then char
            else ' '

-- | Read a response from the arduino.
recv :: SerialPort -> ExceptT ByteString IO ByteString
recv port = go ""
  where
    go :: ByteString -> ExceptT ByteString IO ByteString
    go msg
      | ByteString.length msg >= 3 = pure msg
      | otherwise = do
        bs <- liftIO (Serial.recv port 6)
        go (msg <> ByteString.takeWhile isPrint bs)

arduinoThread :: TQueue Command -> SerialPort -> ExceptT ByteString IO ()
arduinoThread commands port = forever $ do
  command <- atomically (readTQueue commands)
  result <- send port command >> recv port

  case ByteString.take 3 result of
    "ACK" -> pure ()
    _ -> throwError ("arduino responded with an error: " <> result)

new :: MonadSTM m => Maybe FilePath -> m Arduino
new path = liftSTM $ do
  let _serialPath = fromMaybe "/dev/ttyACM0" path
  _screen <- newTVar Blank
  _queue <- newTQueue
  pure Arduino {..}

run ::
  ( MonadIO m,
    MonadReader s m,
    HasArduino s
  ) =>
  m ()
run = do
  vars <- view arduino
  liftIO (async (mcp vars) >>= wait)
  where
    -- Master control process.
    mcp :: Arduino -> IO ()
    mcp vars = do
      exists <- doesFileExist (_serialPath vars)
      if exists
        then do
          _ <- atomically $ flushTQueue (_queue vars)
          t1id <- async (t1 vars)
          t2id <- async (runReaderT t2 vars)
          (_, r) <- waitAnyCatchCancel [t1id, t2id]
          case r of
            Left _ -> threadDelay 1000000 >> mcp vars
            Right _ -> pure ()
        else do
          threadDelay 1000000
          mcp vars
    open :: FilePath -> IO SerialPort
    open =
      ( `Serial.openSerial`
          Serial.defaultSerialSettings {Serial.timeout = 10}
      )
    close :: SerialPort -> IO ()
    close = Serial.closeSerial
    t1 :: Arduino -> IO ()
    t1 vars@Arduino {..} = do
      result <- bracket (open _serialPath) close (runExceptT . arduinoThread _queue)
      case result of
        Right () -> pure () -- Done.
        Left e -> print e >> t1 vars
    t2 :: ReaderT Arduino IO ()
    t2 = forever $ do
      liftIO getCurrentTime >>= draw
      liftIO (threadDelay 1000000)
