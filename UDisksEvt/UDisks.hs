-- udisksevt source file
-- Copyright (C) Vladimir Matveev, 2010
-- Interaction with UDisks daemon signals
module UDisksEvt.UDisks ( wakeDaemon
                        , runSignalHandlers
                        ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Map as M
import qualified Data.Text.Lazy as B
import Data.Int
import Data.List
import Data.Maybe
import Data.Word
import DBus.Bus
import DBus.Client
import DBus.MatchRule as MR
import DBus.Message
import DBus.Types
import System.IO
import System.Environment
import System.Process

import UDisksEvt.Config
import UDisksEvt.DBus
import UDisksEvt.Datatypes
import UDisksEvt.Disk
import UDisksEvt.Log

-- Triggers - match rules mapping
triggers :: [(String, MatchRule)]
triggers = map f $ [ ("added", "DeviceAdded", [])
                   , ("removed", "DeviceRemoved", [])
                   , ("mounted", "DeviceJobChanged",
                      [StringValue 2 "FilesystemMount"])
                   , ("unmounted", "DeviceJobChanged",
                      [StringValue 2 "FilesystemUnmount"])
                   ]
    where
        f (tn, mm, mp) =
            (tn, MatchRule { matchType = Just MR.Signal
                           , matchSender = Nothing
                           , matchDestination = Nothing
                           , matchPath = Just "/org/freedesktop/UDisks"
                           , matchInterface = Just "org.freedesktop.UDisks"
                           , matchMember = Just mm
                           , matchParameters = mp
                           })

-- Run shell command
runShell :: (?st :: UState) => String -> IO ()
runShell cmd = do
    let CVString sh = fromJust $ M.lookup "shell-command" $ cVars $ uConfig ?st
    let shcmd = sh ++ " -c '" ++ cmd ++ "'"
    logRunningCommand shcmd
    runCommand shcmd
    return ()
        
-- Set up UDisks signals handlers
runSignalHandlers :: Configuration -> IO ()
runSignalHandlers conf = do
    client <- systemBusClient
    devs <- newTVarIO M.empty
    -- Set initial state
    let ?st = U conf devs
    -- Set signals based on triggers mapping
    mapM_ (setSignal client) triggers
    where
        setSignal :: (?st :: UState) => Client -> (String, MatchRule) -> IO ()
        setSignal client (rtype, rule) = onSignal client rule (signalDispatcher rtype)

-- Runs trigger on signal
signalDispatcher :: (?st :: UState) => String -> BusName -> Signal -> IO ()
signalDispatcher rtype _ sig = runTrigger rtype obj
    where
        obj = fromJust $ fromVariant $ head $ signalBody sig

-- Extracts trigger actions and executes them sequentially if device is not internal
runTrigger :: (?st :: UState) => String -> ObjectPath -> IO ()
runTrigger rtype obj = do
    logOk $ "Signal caught on trigger " ++ show rtype ++ ":\n\t" ++ show obj
    -- Exit if device is internal - we do not have to act on such devices
    internal <- isDeviceInternal obj
    logOk $ "Is device internal: " ++ show internal
    unless internal $ do
        -- Exit if device is not filesystem
        filesystem <- isDeviceFilesystem obj
        logOk $ "Is device filesystem: " ++ show filesystem
        when filesystem $ do
            -- Necessary delay - otherwise the device isn't mounted properly
            -- before retrieving its properties, resulting in an inability
            -- to get mount point
            when (rtype == "mounted") $ threadDelay 1000000
            logOk $ "Running trigger actions..."
            -- Get trigger actions
            let mctas = getTrigger (uConfig ?st) rtype
            case mctas of
                Nothing -> logTriggerError rtype
                Just ctas -> mapM_ (executeTriggerAction obj) ctas

-- Executes trigger action
executeTriggerAction :: (?st :: UState) => ObjectPath -> ConfigTriggerAction -> IO ()
executeTriggerAction obj cta = do
    dev <- getDeviceInfo obj  -- Retrieve device information
    case cta of
        -- Shell command trigger action
        CTAShellCommand sc -> runShell $ substituteParameters dev sc
        -- Notification trigger action
        CTANotification nbody nsummary nicon ntimeout nurgency ->
            showNotification (substituteParameters dev nbody) (substituteParameters dev nsummary)
                nicon ntimeout nurgency

-- Retrieve device info, either from UDisks or from cache if the former is unavailable
-- It returns something even if info is missing from cache, but logs an error then
-- If info was retrieved from UDisks, add it to cache
getDeviceInfo :: (?st :: UState) => ObjectPath -> IO DeviceInfo
getDeviceInfo obj = do
    (cached, mpm) <- getDevicePropertyMap obj
    case cached of
        -- Device cache was used
        True -> case mpm of
            -- Device isn't in the cache
            -- Log it and return some defaults
            Nothing -> do
                logDeviceInfoError obj
                -- Default properties
                let pm = [ ("DeviceFile", toVariant ("<unknown>" :: String))
                         , ("DeviceIsSystemInternal", toVariant False)
                         , ("IdLabel", toVariant ("<unknown>" :: String))
                         , ("IdUsage", toVariant ("" :: String))
                         , ("DeviceIsMounted", toVariant False)
                         , ("DeviceMountPaths", toVariant emptyArray)
                         ]
                    Just emptyArray = toArray DBusString ([] :: [String])
                return DInfo { diObjectPath = objectPathToString obj
                             , diDeviceFile = "<unknown>"
                             , diProperties = M.fromList pm
                             }
            -- Ok, delete device info from cache and return it
            Just pm -> do
                Just di <- getDeviceInfoCached obj
                atomically $ do
                    devs <- readTVar (uDevices ?st)
                    writeTVar (uDevices ?st) $
                        M.delete (objectPathToString obj) devs
                return di
        -- Cache was not used, construct new device info structure
        -- and add it to cache
        False -> case mpm of
            Nothing ->  -- This is impossible!
                error "Suddenly couldn't get device information!"
            Just pm -> do
                let dobjpath = objectPathToString obj
                    ddevfile = M.lookup "DeviceFile" pm >>= fromVariant
                    dinfo = DInfo { diObjectPath = dobjpath
                                  , diDeviceFile = fromJust $ ddevfile
                                  , diProperties = pm
                                  }
                atomically $ do
                    devs <- readTVar (uDevices ?st)
                    writeTVar (uDevices ?st) $ M.insert dobjpath dinfo devs
                -- Return device information
                return dinfo
                    
-- Performs substring replace to transfer device information to shell commands/notifications
substituteParameters :: (?st :: UState) => DeviceInfo -> String -> String
substituteParameters dev = id {- replace "$DEVICE$" (diDeviceFile dev) .
                           maybe (replace "$MOUNTPATH$" "<not mounted>")
                                 (replace "$MOUNTPATH$") (diMountPoint dev) .
                           replace "$LABEL$" (diLabel dev) .
                           replace "$OBJECTPATH$" (diObjectPath dev) -}

