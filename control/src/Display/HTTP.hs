{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

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
module Display.HTTP
  ( HTTP
  , HasHTTP(hTTP)
  , new
  , message
  , run
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Concurrent.STM.TVar
import Control.Exception (bracket)
import Control.Lens ((&), (?~), (^.), view)
import Control.Lens.TH (makeLenses, makeClassy)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.STM.Class (MonadSTM, liftSTM)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson as Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import qualified Network.Socket as Network
import Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings)
import Servant
import System.Directory (getHomeDirectory, createDirectoryIfMissing, removeFile, doesPathExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeDirectory)

--------------------------------------------------------------------------------
-- Project Imports:
import Display.Timer (Timer)
import Display.Message

--------------------------------------------------------------------------------
-- | A data store that can be fetched over HTTP.
data Store = Store
  { _timer :: Maybe Timer
  , _msg   :: Maybe Text
  } deriving (Generic, Show)

makeLenses ''Store
deriveJSON Aeson.defaultOptions{Aeson.fieldLabelModifier = drop 1} ''Store

--------------------------------------------------------------------------------
-- | Internal reader environment.
data HTTP = HTTP
  { _socketPath :: FilePath
  , _store      :: TVar Store
  }

makeClassy ''HTTP

--------------------------------------------------------------------------------
-- | Create a new reader environment for the HTTP module.
new :: (MonadIO m, MonadSTM m) => m HTTP
new = do
  defaultPath <- (</> ".display-control.sock") <$> liftIO getHomeDirectory
  _socketPath <- fromMaybe defaultPath <$> liftIO (lookupEnv "DISPLAY_HTTP_SOCKET")
  _store <- liftSTM $ newTVar (Store Nothing Nothing)
  pure HTTP{..}

--------------------------------------------------------------------------------
-- | Update state based on the incoming message.
message
  :: ( MonadSTM m
     , MonadReader e m
     , HasHTTP e
     )
     => Message
     -> m ()
message m = do
    var <- view (hTTP.store)
    liftSTM $ modifyTVar' var (update m)
  where
    update :: Message -> Store -> Store
    update m' s =
      case m' of
        TimerStart t    -> s & timer ?~ t
        TimerStop       -> Store Nothing Nothing
        TimerMessage bs -> s & msg ?~ Text.decodeUtf8 bs

--------------------------------------------------------------------------------
run
  :: ( MonadIO m
     , MonadReader e m
     , HasHTTP e
     )
  => m ()
run = do
    env <- view hTTP

    let path = env ^. socketPath
    liftIO $ bracket (open path) (close path) (go env)
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

--------------------------------------------------------------------------------
type TimerAPI = Get '[JSON] Store :<|> "message" :> Get '[JSON] Text

--------------------------------------------------------------------------------
timerAPI :: Proxy TimerAPI
timerAPI = Proxy

--------------------------------------------------------------------------------
timerServer :: ServerT TimerAPI (ReaderT HTTP Handler)
timerServer = getStore :<|> getMessage
  where
    getStore :: ReaderT HTTP Handler Store
    getStore = view (hTTP.store) >>= liftIO . readTVarIO

    getMessage :: ReaderT HTTP Handler Text
    getMessage = do
      s <- getStore
      pure $ fromMaybe "" (s ^. msg)

--------------------------------------------------------------------------------
warpApp :: HTTP -> Application
warpApp env = serve timerAPI timerServer'
  where
    timerServer' :: Server TimerAPI
    timerServer' = hoistServer timerAPI nt timerServer

    nt :: ReaderT HTTP Handler a -> Handler a
    nt = flip runReaderT env
