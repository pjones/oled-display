{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
module Main (main) where

--------------------------------------------------------------------------------
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar
import Control.Exception (bracket)
import Control.Monad (forever, when)
import qualified Data.ByteString.Char8 as ByteString
import System.IO
import System.Serial

--------------------------------------------------------------------------------
main :: IO ()
main = bracket open hClose go
  where
    open :: IO Handle
    open = openSerial "/dev/ttyACM0" B9600 8 One NoParity NoFlowControl

    go :: Handle -> IO ()
    go port = do
      ready <- newEmptyMVar

      t <- async $
        forever $ do
          result <- ByteString.takeWhile ((/= '\r')) <$> ByteString.hGetLine port
          ByteString.hPut stdout "<< "
          ByteString.hPut stdout result
          ByteString.hPut stdout "\n"

          empty <- isEmptyMVar ready
          when (empty && result == "READY") $ do
            ByteString.hPut stdout "-- Release\n"
            putMVar ready True

      _ <- readMVar ready
      ByteString.hPut stdout ">> Sending...\n"
      ByteString.hPutStrLn port "R\r"
      ByteString.hPutStrLn port "S3\r"
      ByteString.hPutStrLn port "T-23:59\r"
      ByteString.hPutStrLn port "F500\r"

      wait t
