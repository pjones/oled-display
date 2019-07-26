{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

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
module Display.Arduino
  ( Arduino
  , new
  , run
  , message
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Exception (bracket)
import Control.Lens (view)
import Control.Lens.TH (makeClassy)
import Control.Monad (forever, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.STM (atomically)
import Control.Monad.STM.Class (MonadSTM, liftSTM)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Char (isPrint, isAscii)
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (UTCTime(..), NominalDiffTime, diffUTCTime, addUTCTime, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.IO (Handle, hClose)
import System.Serial (openSerial)
import qualified System.Serial as Serial

--------------------------------------------------------------------------------
-- Project Imports:
import Display.Message

--------------------------------------------------------------------------------
-- | Information about what the screen is displaying.
data Screen
  = Blank
    -- ^ Not showing anything on the display.

  | Timer UTCTime
    -- ^ Display a timer that started at the given time.

  | Chars ByteString
    -- ^ Unknown ASCII characters.

--------------------------------------------------------------------------------
-- | Commands we can send to the Arduino.
data Command
  = Flash Int
  | Reset
  | TextSize Int
  | TextContent ByteString
  deriving Show

--------------------------------------------------------------------------------
-- | The state for controlling an arduino.
data Arduino = Arduino
  { _screen :: TVar Screen
  , _queue  :: TQueue Command
  }

makeClassy ''Arduino

--------------------------------------------------------------------------------
-- | Process an incoming message.  The screen will be updated the next
-- time that 'draw' is called.
message
  :: ( MonadReader e m
     , HasArduino e
     , MonadSTM m
     )
  => Message
  -> m ()
message msg =
  case update msg of
    Nothing  -> pure ()
    Just scr -> do
      var <- view (arduino.screen)
      liftSTM $ writeTVar var scr

  where
    update :: Message -> Maybe Screen
    update m = case m of
      TimerStart t   -> Just (Timer t)
      TimerStop      -> Just Blank
      TimerMessage _ -> Nothing

--------------------------------------------------------------------------------
-- | Update the display.
draw
  :: forall e m.
     ( MonadReader e m
     , HasArduino e
     , MonadSTM m
     )
  => UTCTime
  -> m ()
draw now = do
  vars <- view arduino

  liftSTM $ do
    scr <- readTVar (_screen vars)

    let commands = case scr of
          Blank    -> [Reset]
          Timer t  -> drawTimer t now
          Chars cs -> drawChars cs

    mapM_ (writeTQueue $ _queue vars) commands

--------------------------------------------------------------------------------
-- | Commands to draw a timer relative to the current time.
drawTimer :: UTCTime -> UTCTime -> [Command]
drawTimer start now =
    drawChars formatted ++ [Flash flash]
  where
    timeLeft :: NominalDiffTime
    timeLeft = pomodoro - diffUTCTime now start

    pomodoro :: NominalDiffTime
    pomodoro = 25 * 60

    flash :: Int
    flash = if timeLeft > 120 then 0 else 500

    formatted :: ByteString
    formatted = ByteString.pack $
      formatTime defaultTimeLocale "%M:%S" $
      {- Next bit only needed for older version of the time package -}
      addUTCTime timeLeft (UTCTime (ModifiedJulianDay 0) (fromIntegral (0::Int)))

--------------------------------------------------------------------------------
-- | Commands to draw arbitrary text.
drawChars :: ByteString -> [Command]
drawChars chars =
    [ TextSize (size chars)
    , TextContent chars
    ]
  where
    size :: ByteString -> Int
    size bs | ByteString.length bs < 6  = 4
            | ByteString.length bs < 10 = 3
            | ByteString.length bs < 20 = 2
            | otherwise                 = 1

--------------------------------------------------------------------------------
-- | Send a command to the arduino.
send :: Handle -> Command -> IO ()
send h c = Builder.hPutBuilder h (chars <> "\n")
  where
    chars :: Builder
    chars = case c of
      Flash n        -> "F" <> Builder.intDec n
      Reset          -> "R"
      TextSize n     -> "S" <> Builder.intDec n
      TextContent bs -> "T" <> Builder.byteString (clean bs)

    clean :: ByteString -> ByteString
    clean = ByteString.map clean'
      where clean' char = if isAscii char && isPrint char
                            then char
                            else ' '

--------------------------------------------------------------------------------
-- | Read a response from the arduino.
recv :: Handle -> IO ByteString
recv h = do
  msg <- ByteString.hGetLine h

  if ByteString.null msg
    then recv h
    else pure $ ByteString.takeWhile (/= '\r') msg

--------------------------------------------------------------------------------
arduinoThread :: TQueue Command -> Handle -> IO ()
arduinoThread commands handle = do
  putStrLn "waiting for READY from arduino"
  msg <- recv handle

  unless (msg == "READY") $
    -- FIXME: Handle this better.
    error ("expected READY but got " ++ show msg)

  forever $ do
    command <- atomically (readTQueue commands)
    putStrLn ("sending command " ++ show command)
    send handle command

    result <- recv handle
    putStrLn ("received answer " ++ show result)

    case ByteString.take 3 result of
      "ACK" -> pure ()
      _     -> error "arduino responded with an error"

--------------------------------------------------------------------------------
new :: (MonadSTM m) => m Arduino
new = liftSTM $ do
  _screen <- newTVar Blank
  _queue  <- newTQueue
  pure Arduino{..}

--------------------------------------------------------------------------------
run
  :: ( MonadIO m
     , MonadReader s m
     , HasArduino s
     )
  => m ()
run = do
  vars <- view arduino

  t1id <- liftIO $ async (t1 $ _queue vars)
  t2id <- liftIO $ async (runReaderT t2 vars)

  liftIO $ wait t1id
  liftIO $ wait t2id

  where
    t1 :: TQueue Command -> IO ()
    t1 q = do
      let open = openSerial "/dev/ttyACM0"
                            Serial.B9600
                            8
                            Serial.One
                            Serial.NoParity
                            Serial.NoFlowControl

      bracket open hClose (arduinoThread q)

    t2 :: ReaderT Arduino IO ()
    t2 = forever $ do
      liftIO getCurrentTime >>= draw
      liftIO (threadDelay 1000000)
