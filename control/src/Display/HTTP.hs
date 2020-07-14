{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

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
-- Examples:
--
-- @
-- curl --silent --unix-socket ~/.display-control.sock 'http://localhost/message' | jq --raw-output
--
-- curl --silent --unix-socket ./display-control.sock http://localhost/stream
-- @
module Display.HTTP
  ( HTTP,
    HasHTTP (hTTP),
    new,
    message,
    run,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, waitAnyCatchCancel)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TSem as STM
import Control.Exception (bracket)
import Control.Lens ((&), (?~), (^.), view)
import Control.Lens.TH (makeClassy, makeLenses)
import Control.Monad.STM.Class (MonadSTM, liftSTM)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (getCurrentTime)
import Display.Message
import Display.Timer (Timer)
import qualified Display.Timer as Timer
import GHC.Generics (Generic)
import qualified Network.Socket as Network
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket)
import Servant
import qualified Servant.Types.SourceT as Servant
import System.Directory (createDirectoryIfMissing, doesPathExist, getHomeDirectory, removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeDirectory)

-- | A data store that can be fetched over HTTP.
data Store = Store
  { _timer :: Maybe Timer,
    _msg :: Maybe Text
  }
  deriving (Generic, Show)

makeLenses ''Store
deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = drop 1} ''Store

-- | Internal reader environment.
data HTTP = HTTP
  { _socketPath :: FilePath,
    _store :: STM.TVar Store,
    _formatted :: STM.TChan Text,
    _signal :: STM.TSem
  }

makeClassy ''HTTP

-- | Create a new reader environment for the HTTP module.
new :: forall m. (MonadIO m, MonadSTM m) => Maybe FilePath -> m HTTP
new cmdLinePath = do
  _socketPath <- calcPath
  liftSTM $ do
    _store <- STM.newTVar (Store Nothing Nothing)
    _formatted <- STM.newBroadcastTChan
    _signal <- STM.newTSem 0
    pure HTTP {..}
  where
    calcPath :: m FilePath
    calcPath = do
      defaultPath <- (</> ".display-control.sock") <$> liftIO getHomeDirectory
      envPath <- liftIO (lookupEnv "DISPLAY_HTTP_SOCKET")
      pure (fromMaybe defaultPath (cmdLinePath <|> envPath))

-- | Update state based on the incoming message.
message ::
  ( MonadSTM m,
    MonadReader e m,
    HasHTTP e
  ) =>
  Message ->
  m ()
message m = do
  vars <- view hTTP
  liftSTM $ do
    STM.modifyTVar' (vars ^. store) (update m)
    STM.signalTSem (vars ^. signal)
  where
    update :: Message -> Store -> Store
    update m' s =
      case m' of
        TimerStart t -> s & timer ?~ t
        TimerStop -> Store Nothing Nothing
        TimerMessage bs -> s & msg ?~ Text.decodeUtf8 bs

run ::
  ( MonadIO m,
    MonadReader e m,
    HasHTTP e
  ) =>
  m ()
run = do
  env <- view hTTP
  let path = env ^. socketPath
  t0 <- liftIO $ async (formatThread env)
  t1 <- liftIO $ async (bracket (open path) (close path) (go env))
  liftIO (void $ waitAnyCatchCancel [t0, t1])
  where
    open :: FilePath -> IO Network.Socket
    open path = do
      createDirectoryIfMissing True (takeDirectory path)
      sock <- Network.socket Network.AF_UNIX Network.Stream 0
      Network.bind sock $ Network.SockAddrUnix path
      Network.listen sock Network.maxListenQueue
      pure sock
    close :: FilePath -> Network.Socket -> IO ()
    close path socket = do
      Network.close socket
      doesPathExist path >>= flip when (removeFile path)
    go :: HTTP -> Network.Socket -> IO ()
    go env socket = runSettingsSocket defaultSettings socket (warpApp env)

type TimerAPI =
  Get '[JSON] Store
    :<|> "message" :> Get '[JSON] Text
    :<|> "stream" :> StreamGet NewlineFraming PlainText (SourceIO Text)

timerAPI :: Proxy TimerAPI
timerAPI = Proxy

timerServer :: ServerT TimerAPI (ReaderT HTTP Handler)
timerServer = getStore :<|> getMessage :<|> getStream
  where
    getStore :: ReaderT HTTP Handler Store
    getStore = view (hTTP . store) >>= readTVarIO
    getMessage :: ReaderT HTTP Handler Text
    getMessage = do
      s <- getStore
      pure $ fromMaybe "" (s ^. msg)
    getStream :: ReaderT HTTP Handler (SourceIO Text)
    getStream = do
      broadcast <- view (hTTP . formatted)
      chan <- liftIO $ atomically (STM.dupTChan broadcast)
      pure (streamServer chan)

streamServer :: MonadIO m => STM.TChan Text -> Servant.SourceT m Text
streamServer chan =
  Servant.fromAction
    (const False)
    (liftIO $ atomically (STM.readTChan chan))

formatStore :: Store -> IO Text
formatStore Store {..} = do
  let desc = maybe [] (one) _msg
  time <- maybe (pure []) (fmap (\t -> one ("[" <> t <> "]")) . formatTimer) _timer
  pure $ Text.intercalate " " (desc <> time)
  where
    formatTimer :: Timer -> IO Text
    formatTimer t = do
      now <- getCurrentTime
      pure (decodeUtf8 (Timer.render t now))

formatThread :: HTTP -> IO ()
formatThread vars = do
  t0 <- async (update "")
  t1 <- async tick
  void $ waitAnyCatchCancel [t0, t1]
  where
    -- The update thread.
    update :: Text -> IO ()
    update prev = do
      st <- STM.atomically $ do
        STM.waitTSem (vars ^. signal)
        STM.readTVar (vars ^. store)
      fmt <- formatStore st
      when (prev /= fmt) $
        STM.atomically (STM.writeTChan (vars ^. formatted) fmt)
      update fmt
    -- Trigger a reformat every second.
    tick :: IO ()
    tick = forever $ do
      threadDelay 1000000
      STM.atomically (STM.signalTSem (vars ^. signal))

warpApp :: HTTP -> Application
warpApp env = serve timerAPI timerServer'
  where
    timerServer' :: Server TimerAPI
    timerServer' = hoistServer timerAPI nt timerServer
    nt :: ReaderT HTTP Handler a -> Handler a
    nt = flip runReaderT env
