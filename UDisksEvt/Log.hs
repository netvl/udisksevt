-- udisksevt source file
-- Copyright (C) Vladimir Matveev, 2010
-- Logging routines
module UDisksEvt.Log where

import DBus.Message
import DBus.Types
import System.IO

import qualified Data.Text.Lazy as B

-- Print simple log message; now using only stdout
logOk :: String -> IO ()
logOk = putStrLn

-- Print error message; now using only stderr
logError :: String -> IO ()
logError = hPutStrLn stderr

-- Log DBus error; now used only in daemon startup routine
logDBusError :: Error -> IO ()
logDBusError err = logError $ "Couldn't start up UDisks daemon:\n\t" ++ show err

-- Log UDisks daemon startup
logDaemonStartup :: MethodReturn -> IO ()
logDaemonStartup _ = logOk $ "UDisks daemon successfully started!"

-- Trying to launch an unspecified trigger
logTriggerError :: String -> IO ()
logTriggerError rtype = logError $ "Trying to launch a trigger undefined in configuration;" ++ 
                  "it's safe do this unless you need any actions:\n\t" ++ show rtype

-- Can't get device info neither from UDisks nor from cache
logDeviceInfoError :: ObjectPath -> IO ()
logDeviceInfoError obj = logError $
    "Cannot get device info; it's not in UDisks database nor in device cache:\n\t" ++ show obj

-- Log command running
logRunningCommand :: String -> IO ()
logRunningCommand cmd = logOk $ "Running command:\n\t" ++ show cmd

-- Print that there was an error with notification sending
logNotifyError :: String -> String -> Error -> IO ()
logNotifyError summary body err = do
    let errname = B.unpack $ strErrorName $ errorName err
    logError $ "Error sending notification: " ++ errname ++
        ":\n\t" ++ show summary ++ " " ++ show body

-- Print that notification sending was ok
logNotifyOk :: String -> String -> String -> MethodReturn -> IO ()
logNotifyOk summary body icon _ =
    logOk $ "Successfully sent notification:\n\t" ++ show summary ++ " " ++ show body ++ 
    " " ++ show icon

