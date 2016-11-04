{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent            (threadDelay)
import Control.Concurrent.STM.TQueue
import Protolude                     hiding (bracket)

import LoggerThread

main :: IO ()
main = do
  args <- getArgs
  putText ("command line arguments: " <> show args)
  numCapabilities <- getNumCapabilities
  putText ("number of cores: " <> show numCapabilities)
  withLoggerThread mainThread

mainThread :: TQueue Text -> IO ()
mainThread logChannel = do
  toLog logChannel "logginng stuff and again"
  threadDelay 1000000000
  toLog logChannel "logginng stuff and again"
  threadDelay 1000000000
  toLog logChannel "logginng stuff and again"
  threadDelay 1000000000
  toLog logChannel "logginng stuff and again"
  threadDelay 1000000000
  toLog logChannel "logginng stuff and again"
