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
--        getDevicePropertyMap obj >>= print . snd
        logOk $ "Is device filesystem: " ++ show filesystem
        when filesystem $ do
            -- Necessary delay - otherwise the device isn't mounted properly
            -- before retrieving its properties, resulting in an inability
            -- to get mount point
            when (rtype == "mounted") $ threadDelay 1000000
            logOk $ "Running trigger actions..."
            -- Get trigger actions and perform them
            let mctas = getTrigger (uConfig ?st) rtype
            case mctas of
                Nothing -> logTriggerError rtype
                Just ctas -> mapM_ (executeTriggerAction obj) ctas
            -- Delete device info from cache if it is removed
            when (rtype == "removed") $ atomically $ do
                devs <- readTVar (uDevices ?st)
                writeTVar (uDevices ?st) $ M.delete (objectPathToString obj) devs
                

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
        getVarPositions :: String -> [(Int, Int, String)]
        getVarPositions s = (\(x, _, _) -> x) $ execState (f s 0 False) ([], "", 0)
            where
                f :: String -> Int -> Bool
                     -> State ([(Int, Int, String)], String, Int) ()
                f "" _ False = return ()
                f "" _ True = put ([], "", 0)
                f ('$':s) i False = do
                    modify $ \(lst, v, ib) -> (lst, "", i)
                    f s (i+1) True
                f ('$':s) i True = do
                    modify $ \(lst, v, ib) -> ((ib, i, v):lst, "", 0)
                    f s (i+1) False
                f (c:s) i False =
                    f s (i+1) False
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

-- Converts arbitrary Variant value to its string representation
-- Just shows plain values, recursively shows Variant values,
-- shows special values as strings and takes the first element of
-- dictionaries and arrays
variantToString :: Variant -> String
variantToString v = f (variantType v)
    where  -- These are different cases of Variant types
        f DBusString =
            fromJust $ (fromVariant v :: Maybe String)
        f DBusObjectPath =
            objectPathToString $ fromJust $ (fromVariant v :: Maybe ObjectPath)
        f DBusSignature =
            B.unpack $ strSignature $ fromJust $ (fromVariant v :: Maybe Signature)
        f DBusVariant =
            variantToString $ fromJust $ (fromVariant v :: Maybe Variant)
        f (DBusArray _) =
            maybe "" variantToString $ listToMaybe $
            arrayItems $ fromJust $ fromVariant $ v
        f (DBusDictionary _ _) =
            maybe "" variantToString $ fmap snd $ listToMaybe $
            dictionaryItems $ fromJust $ fromVariant v
        f t = case t of
            DBusBoolean -> show $ fromJust $ (fromVariant v :: Maybe Bool)
            DBusByte    -> show $ fromJust $ (fromVariant v :: Maybe Word8)
            DBusInt16   -> show $ fromJust $ (fromVariant v :: Maybe Int16)
            DBusInt32   -> show $ fromJust $ (fromVariant v :: Maybe Int32)
            DBusInt64   -> show $ fromJust $ (fromVariant v :: Maybe Int64)
            DBusWord16	-> show $ fromJust $ (fromVariant v :: Maybe Word16)
            DBusWord32	-> show $ fromJust $ (fromVariant v :: Maybe Word32)
            DBusWord64	-> show $ fromJust $ (fromVariant v :: Maybe Word64)
            DBusDouble	-> show $ fromJust $ (fromVariant v :: Maybe Double)

    {- replace "$DEVICE$" (diDeviceFile dev) .
    maybe (replace "$MOUNTPATH$" "<not mounted>")                           
    (replace "$MOUNTPATH$") (diMountPoint dev) .
    replace "$LABEL$" (diLabel dev) .
    replace "$OBJECTPATH$" (diObjectPath dev) -}
