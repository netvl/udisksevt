-- udisksevt source file
-- Copyright (C) Vladimir Matveev, 2010
-- Interaction with UDisks daemon signals
module UDisksEvt.UDisks ( wakeDaemon
                        , runSignalHandlers
                        ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.State
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

import qualified Data.Map as M
import qualified Data.Text.Lazy as B

import UDisksEvt.Config
import UDisksEvt.DBus
import UDisksEvt.Datatypes
import UDisksEvt.Disk
import UDisksEvt.Log
import UDisksEvt.Utils

-- Triggers - match rules mapping
{- triggers = map f $ [ ("added",     DTFlashMemory, "DeviceAdded",      [])
                   , ("added",     DTOpticalDisc, "DeviceChanged",    [])
                   , ("removed",   DTFlashMemory, "DeviceRemoved",    [])
                   , ("mounted",   DTFlashMemory, "DeviceJobChanged", [StringValue 2 "FilesystemMount"])
                   , ("unmounted", DTFlashMemory, "DeviceJobChanged", [StringValue 2 "FilesystemUnmount"])
                   ]
    where
        f (tn, dt, mm, mp) =
            (tn, dt, MatchRule { matchType = Just MR.Signal
                               , matchSender = Nothing
                               , matchDestination = Nothing
                               , matchPath = Just "/org/freedesktop/UDisks"
                               , matchInterface = Just "org.freedesktop.UDisks"
                               , matchMember = Just mm
                               , matchParameters = mp
                               }) -}

udisksSignalMatchRule :: MatchRule
udisksSignalMatchRule = MatchRule { matchType = Just MR.Signal
                                  , matchSender = Nothing
                                  , matchDestination = Nothing
                                  , matchPath = Just "/org/freedesktop/UDisks"
                                  , matchInterface = Just "org.freedesktop.UDisks"
                                  , matchMember = Nothing
                                  , matchParameters = []
                                  }


-- Run shell command
runShell :: (?st :: UState) => String -> IO ()
runShell cmd = do
    let CVString sh = fromJust $ M.lookup "shell-command" $ cVars $ uConfig ?st
        shcmd = sh ++ " -c '" ++ cmd ++ "'"
    logRunningCommand shcmd
--    runCommand shcmd
    devnull <- openFile "/dev/null" ReadWriteMode 
    _ <- runProcess sh ["-c", cmd] Nothing Nothing (Just devnull) (Just devnull) (Just devnull)
    return ()
        
-- Set up UDisks signals handlers
runSignalHandlers :: Configuration -> IO ()
runSignalHandlers conf = do
    client <- systemBusClient
    devs <- newTVarIO M.empty
    -- Set initial state
    let ?st = U conf devs
    -- Set signal handler routine
    onSignal client udisksSignalMatchRule signalHandler

-- Performs various actions depending on signal
signalHandler :: (?st :: UState) => BusName -> Signal -> IO ()
signalHandler _ (signalMember `fork` signalBody -> (sname, sbody)) = do
    mrname <- case sname of
        "DeviceAdded" -> return $ Just "added"
        "DeviceRemoved" -> return $ Just "removed"
        "DeviceJobChanged" -> case jobid of
            "FilesystemMount" -> return $ Just "mounted"
            "FilesystemUnmount" -> return $ Just "unmounted"
            _ -> return Nothing
        "DeviceChanged" -> do
            (isoptical, isinserted) <- isDeviceOpticalDisc obj
            if isoptical
                then if isinserted
                     then return $ Just "added"
                     else return $ Just "removed"
                else return $ Nothing
    case mrname of
        Nothing -> return ()  -- Do nothing
        Just rname -> runTrigger rname obj  -- Process the signal
    where
        obj :: ObjectPath
        obj = fromJust $ fromVariant (head sbody)  -- Extract object name
        jobid :: String
        jobid = fromJust $ fromVariant (sbody !! 3)  -- Extract job id

-- Extracts trigger actions and executes them sequentially if device is not internal
runTrigger :: (?st :: UState) => String -> ObjectPath -> IO ()
runTrigger rname obj = do
    logOk $ "Signal caught on trigger " ++ show rname ++ ":\n\t" ++ show obj
    -- Check device whether we have to process the device
    checkDevice obj >>= \v -> when v $ do
        -- Necessary delay - otherwise the device isn't mounted properly
        -- before retrieving its properties, resulting in an inability
        -- to get mount point
        when (rname == "mounted") $ threadDelay 1000000
        logOk $ "Running trigger actions..."
        -- Get trigger actions and perform them
        let mctas = getTrigger (uConfig ?st) rname
        case mctas of
            Nothing -> logTriggerError rname
            Just ctas -> mapM_ (executeTriggerAction obj) ctas
        -- Delete device info from cache if it is removed
        when (rname == "removed") $ atomically $ do
            devs <- readTVar (uDevices ?st)
            writeTVar (uDevices ?st) $ M.delete (objectPathToString obj) devs

-- Check necessary device parameters
checkDevice :: (?st :: UState) => ObjectPath -> IO Bool
checkDevice obj = do
    -- Check if device is internal
    internal <- isDeviceInternal obj
    logOk $ "Is device internal: " ++ show internal
    if internal  -- False if internal
        then return False
        else do
            -- Check if device is filesystem
            filesystem <- isDeviceFilesystem obj
            logOk $ "Is device filesystem: " ++ show filesystem
            if not filesystem
                then return False  -- False if device is not a filesystem
                else return True

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
            -- Ok, build structure again (less overhead) and return it
            Just pm -> do
                let Just ddevfile = M.lookup "DeviceFile" pm >>= fromVariant
                    dinfo = DInfo { diObjectPath = objectPathToString obj
                                  , diDeviceFile = ddevfile
                                  , diProperties = pm
                                  }
                return dinfo
        -- Cache was not used, construct new device info structure
        -- and add it to cache
        False -> case mpm of
            Nothing ->  -- This is impossible!
                error "Suddenly couldn't get device information!"
            Just pm -> do
                let dobjpath = objectPathToString obj
                    Just ddevfile = M.lookup "DeviceFile" pm >>= fromVariant
                    dinfo = DInfo { diObjectPath = dobjpath
                                  , diDeviceFile = ddevfile
                                  , diProperties = pm
                                  }
                atomically $ do
                    devs <- readTVar (uDevices ?st)
                    writeTVar (uDevices ?st) $ M.insert dobjpath dinfo devs
                -- Return device information
                return dinfo
                    
-- Performs substring replace to set device info into notification/shell command
-- string
substituteParameters :: DeviceInfo -> String -> String
substituteParameters dev s = doReplaces s $ getVarPositions s
    where
        -- Retrieves left and right bounds and values of all $VAR$-variables
        -- in specified string
        -- Simple state machine
        getVarPositions :: String -> [(Int, Int, String)]
        getVarPositions s = (\(x, _, _) -> x) $ execState (f s 0 False) ([], "", 0)
            where
                f :: String -> Int -> Bool
                     -> State ([(Int, Int, String)], String, Int) ()
                -- Correct ending, no opened variables
                f "" _ False = return ()
                -- Incorrect ending, there is an opened variable
                f "" _ True = put ([], "", 0)
                -- Beginning of variable
                f ('$':s) i False = do
                    modify $ \(lst, v, ib) -> (lst, "", i)
                    f s (i+1) True
                -- Ending of variable
                f ('$':s) i True = do
                    modify $ \(lst, v, ib) -> ((ib, i, v):lst, "", 0)
                    f s (i+1) False
                -- The string outside variable
                f (c:s) i False =
                    f s (i+1) False
                -- Variable data
                f (c:s) i True = do
                    modify $ \(lst, v, ib) -> (lst, v ++ [c], ib)
                    f s (i+1) True

        -- Replacing loop
        doReplaces :: String -> [(Int, Int, String)] -> String
        doReplaces s [] = s
        doReplaces s ((ib, ie, v):lst) = doReplaces (doReplace ib ie v s) lst

        -- Perform the replace
        doReplace :: Int -> Int -> String -> String -> String
        doReplace ib ie v s = before ++ replacement ++ after
            where
                (before, _) = splitAt ib s  -- Split at the first '$' sign
                (_,  after) = splitAt (ie+1) s  -- At the second
                replacement = case M.lookup v (diProperties dev) of
                    Nothing -> ""
                    Just val -> variantToString val
