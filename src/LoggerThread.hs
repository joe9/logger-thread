{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module LoggerThread
  ( toLog
  , withLoggerThread
  ) where

import Control.Concurrent.STM.TQueue
import Control.Exception.Safe
import Control.Monad.STM             (atomically)
import Data.Text
import Protolude                     hiding (bracket)
import System.Log.FastLogger

-- import Data.Time                     (FormatTime, defaultTimeLocale,
--                                       formatTime, getZonedTime)
-- toLog :: Text -> Text -> IO ()
-- toLog prefix s =
--   filename prefix >>=
--   (\f ->
--      appendFile (cs f)
--                 (s <> (singleton '\n')))
toLog :: TQueue Text -> Text -> IO ()
toLog chan s = atomically (writeTQueue chan (s <> singleton '\n'))

-- logFilename :: Text -> Text -> IO Text
-- logFilename logDirectory prefix = do
--   z <- getZonedTime
--   return $ logDirectory <> "/" <> prefix <> dateString z <> ".log"
-- logDirectory :: FilePath
-- logDirectory = dataDirectory <> "/log/"
-- dateString
--   :: (FormatTime a)
--   => a -> Text
-- dateString = show . formatTime defaultTimeLocale "%Y%m%d"
withLoggerThread :: (TQueue Text -> IO ()) -> IO ()
withLoggerThread mainThread = do
  logChannel <- atomically newTQueue
  bracket
    (async (loggerThread logChannel))
    (\loggerThreadAsync -> do
       putText "after the main thread, stop the loggerThread"
       -- dummy text to unblock the logger channel
       threadDelay (1000 * 1000)
       cancel loggerThreadAsync
       putText "cancelled the loggerThread"
       wait loggerThreadAsync
       putText "waited for the loggerThread")
    (\loggerThreadAsync -> do
       link loggerThreadAsync
       withException
         (mainThread logChannel)
         (\e -> do
            putText "exception in main thread"
            (print :: SomeException -> IO ()) e
            putText "after the main thread, stop the loggerThread"))

--   1 MiB = 1 mebibyte = 1,0242 bytes = 1,048,576 bytes
--   100 MiB = 1,048,576 * 100 bytes
loggerThread :: TQueue Text -> IO ()
loggerThread chan =
  withFastLogger
    (LogStderr 1048576)
    (\f ->
       withException
         (readAndLog chan f)
         (\e -> do
            putText "exception in logger thread"
            (print :: SomeException -> IO ()) e
            shutdownLogging chan f))

shutdownLogging :: TQueue Text -> (LogStr -> IO a) -> IO ()
shutdownLogging chan f = do
  putText "in shutdownLogging"
  maybeValue <- atomically (tryReadTQueue chan)
  case maybeValue of
    Just t -> (f . (toLogStr :: Text -> LogStr)) t >> shutdownLogging chan f
    Nothing -> putText "LoggerThread stopped reading, exiting" >> return ()

readAndLog :: TQueue Text -> (LogStr -> IO a) -> IO ()
readAndLog chan f =
  atomically (readTQueue chan) >>= f . (toLogStr :: Text -> LogStr) >>
  readAndLog chan f
-- log rotation is a system function, not an application function
-- should not be doing this here
-- not sure how to use this. If I use it readAndLog, it gives this
-- message
-- GetMarketsServer: /home/j/var/betfair//log/aping-20160820-betfair.log: openFile: resource busy (file is locked)
-- checkAndRotate :: FileLogSpec -> IO ()
-- checkAndRotate spec =
--   do size <- getFileSize (log_file spec)
--      when (size > log_file_size spec)
--           (rotate spec)
-- getFileSize
--   :: BasicPrelude.FilePath -> IO Integer
-- getFileSize path = withFile path ReadMode hFileSize
