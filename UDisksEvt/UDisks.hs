-- udisksevt source file
-- Copyright (C) DPX-Infinity, 2010
-- Module for communicating with UDisks through D-Bus

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
import UDisksEvt.Datatypes

-- Triggers - match rules mapping
triggers = [ ("added", MatchRule { matchType = Just MR.Signal
                                 , matchSender = Nothing
                                 , matchDestination = Nothing
                                 , matchPath = Just "/org/freedesktop/UDisks"
                                 , matchInterface = Just "org.freedesktop.UDisks"
                                 , matchMember = Just "DeviceAdded"
                                 , matchParameters = []
                                 })
           , ("removed", MatchRule { matchType = Just MR.Signal
                                   , matchSender = Nothing
                                   , matchDestination = Nothing
                                   , matchPath = Just "/org/freedesktop/UDisks"
                                   , matchInterface = Just "org.freedesktop.UDisks"
                                   , matchMember = Just "DeviceRemoved"
                                   , matchParameters = []
                                   })
           , ("mounted", MatchRule { matchType = Just MR.Signal
                                   , matchSender = Nothing
                                   , matchDestination = Nothing
                                   , matchPath = Just "/org/freedesktop/UDisks"
                                   , matchInterface = Just "org.freedesktop.UDisks"
                                   , matchMember = Just "DeviceJobChanged"
                                   , matchParameters =
                                       [StringValue 2 "FilesystemMount"]
                                   })
           , ("unmounted", MatchRule { matchType = Just MR.Signal
                                     , matchSender = Nothing
                                     , matchDestination = Nothing
                                     , matchPath = Just "/org/freedesktop/UDisks"
                                     , matchInterface = Just "org.freedesktop.UDisks"
                                     , matchMember = Just "DeviceJobChanged"
                                     , matchParameters =
                                         [StringValue 2 "FilesystemUnmount"]
                                     })
           ]

-- Proxy for /org/freedesktop/UDisks object
udisksProxy = Proxy (RemoteObject "org.freedesktop.UDisks" "/org/freedesktop/UDisks")
              "org.freedesktop.UDisks"

-- Proxy for arbitrary device for device methods
deviceProxy dev = Proxy (RemoteObject "org.freedesktop.UDisks" dev)
                  "org.freedesktop.UDisks.Device"

-- Proxy for arbitrary device for device properties
devicePropertyProxy dev = Proxy (RemoteObject "org.freedesktop.UDisks" dev)
                         "org.freedesktop.DBus.Properties"

-- Notification daemon proxy
notificationProxy =
    Proxy (RemoteObject "org.freedesktop.Notifications" "/org/freedesktop/Notifications")
    "org.freedesktop.Notifications"

-- Get system bus Client object
systemBusClient = mkClient =<< getSystemBus

-- Get session bus Client object
sessionBusClient = mkClient =<< getSessionBus

-- Print simple log message; now using only stdout
logOk :: String -> IO ()
logOk = putStrLn

-- Print error message; now using only stderr
logError :: String -> IO ()
logError = hPutStrLn stderr

-- Log DBus error; now used only in daemon startup routine
logDBusError :: Error -> IO ()
logDBusError err = logError $ "Coulnd't startup UDisks daemon:\n\t" ++ show err

-- Log UDisks daemon startup
logDaemonStartup :: MethodReturn -> IO ()
logDaemonStartup _ = logOk $ "UDisks daemon successfully started!"

-- Trying to launch an unspecified trigger
logTriggerError :: String -> IO ()
logTriggerError rtype = logError $ "Trying to launch trigger undefined in configuration;" ++ 
                  "it's safe do this if you don't need any actions:\n\t" ++ show rtype

-- Can't get device info neither from UDisks nor from cache
logDeviceInfoError :: ObjectPath -> IO ()
logDeviceInfoError obj = logError $
    "Cannot get device info: it's not in UDisks database nor in device cache:\n\t" ++ show obj

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

-- Run shell command
runShell :: (?st :: UState) => String -> IO ()
runShell cmd = do
    let CVString sh = fromJust $ M.lookup "shell-command" $ cVars $ uConfig ?st
    let shcmd = sh ++ " -c '" ++ cmd ++ "'"
    logRunningCommand shcmd
    runCommand shcmd
    return ()

-- Show notification using D-Bus org.freedesktop.Notifications server, if present
showNotification :: (?st :: UState) => String -> String -> String -> Int -> NotificationUrgency
                    -> IO ()
showNotification body summary icon timeout urgency = do
    client <- sessionBusClient
    let actions = fromJust $ toArray DBusString ([] :: [String])
    let nurgency = case urgency of
            NULow -> 0 :: Word8
            NUNormal -> 1
            NUCritical -> 2
    let hints = fromJust $
                dictionaryFromItems DBusString DBusVariant
                    [(toVariant ("urgency" :: String), toVariant $ toVariant nurgency)]
    homepath <- getEnv "HOME"
    let CVString ricon = if icon /= "default"
                         then CVString icon
                         else fromJust $ M.lookup "default-notification-icon" $ cVars $
                              uConfig ?st
    let ricon' = replace "$HOME$" homepath ricon
    callProxy client notificationProxy "Notify" [] [ toVariant ("UDisksEvt" :: String)
                                                   , toVariant (0 :: Word32)
                                                   , toVariant ricon'
                                                   , toVariant summary
                                                   , toVariant body
                                                   , toVariant actions
                                                   , toVariant hints
                                                   , toVariant (fromIntegral timeout :: Int32)
                                                   ]
        (logNotifyError summary body) (logNotifyOk summary body ricon')
    return ()

-- Checks if device is system internal
isDeviceInternal :: (?st :: UState) => ObjectPath -> IO Bool
isDeviceInternal obj = do
    client <- systemBusClient
    response <- callProxyBlocking client (devicePropertyProxy obj) "Get" [] $
        map toVariant (["org.freedesktop.UDisks.Device", "DeviceIsSystemInternal"] :: [String])
    case response of
        Left _ -> do  -- Error, trying to get value from cache
            dev <- getCachedDeviceInfo obj
            case dev of
                Nothing -> return False
                Just d -> return $ diInternal d
        Right resp ->  -- Ok, get actual value
            let Just isinternal = fromVariant =<< (fromVariant $ head $ messageBody resp)
            in return isinternal

-- Checks is device is mountable filesystem
isDeviceFilesystem :: (?st :: UState) => ObjectPath -> IO Bool
isDeviceFilesystem obj = do
    client <- systemBusClient
    response <- callProxyBlocking client (devicePropertyProxy obj) "Get" [] $
        map toVariant (["org.freedesktop.UDisks.Device", "IdUsage"] :: [String])
    case response of
        Left _ -> do  -- Error, trying to get value from cache
            dev <- getCachedDeviceInfo obj
            case dev of
                Nothing -> return False
                Just d -> return $ diFSystem d
        Right resp ->  -- Ok, get actual value
            let Just devtype = fromVariant =<< (fromVariant $ head $ messageBody resp)
            in return $ devtype == ("filesystem" :: String)

-- http://bluebones.net/2007/01/replace-in-haskell/ - Joseph's function
replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace [] _ list = list
replace oldSub newSub list = _replace list where
	_replace list@(h:ts) = if isPrefixOf oldSub list
		then newSub ++ _replace (drop len list)
		else h : _replace ts
	_replace [] = []
	len = length oldSub
        
-- Wake UDisks daemon with a request
wakeDaemon :: IO ()
wakeDaemon = do
    client <- systemBusClient
    callProxy client udisksProxy "EnumerateDevices" [] [] logDBusError logDaemonStartup

enumerateDevices :: IO [String]
enumerateDevices = do
    client <- systemBusClient
    let proxy = udisksProxy
    response <- callProxyBlocking_ client udisksProxy "EnumerateDevices" [] []
    let rdata = head $ messageBody response
    let Just lst1 = fromArray =<< fromVariant rdata
    return $ map (B.unpack . strObjectPath) lst1

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
signalDispatcher rtype bname sig = runTrigger rtype obj
    where
        obj = fromJust $ fromVariant $ head $ signalBody sig

-- Extracts trigger actions and executes them sequentially if device is not internal
runTrigger :: (?st :: UState) => String -> ObjectPath -> IO ()
runTrigger rtype obj = do
    logOk $ "Signal caught on trigger " ++ show rtype ++ ":\n\t" ++ show obj
    -- Exit if device is internal - we do not have to act on such devices
    internal <- isDeviceInternal obj
    logOk $ "Is device internal: " ++ show internal
    when (not internal) $ do
        -- Exit if device is not filesystem
        filesystem <- isDeviceFilesystem obj
        logOk $ "Is device filesystem: " ++ show filesystem
        when filesystem $ do
            when (rtype == "mounted") $ threadDelay 1000000
            logOk $ "Running trigger actions..."
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

-- Retrieve device info, either from UDisks or from cache if the latest is unavailable
-- It returns something even if info is missing from cache, though logs an error then
getDeviceInfo :: (?st :: UState) => ObjectPath -> IO DeviceInfo
getDeviceInfo obj = do
    client <- systemBusClient
    -- Get all properties of device
    -- There is *many* of them
    response <- callProxyBlocking client (devicePropertyProxy obj) "GetAll" []
                [toVariant ("org.freedesktop.UDisks.Device" :: String)]
    mprops <- case response of
        -- If request failed (e.g. in case of detached device), try to get info from cache
        Left _ -> return Nothing
        -- Request is successful, trying to retrieve properties map
        Right resp -> let rdata = head $ messageBody resp
                      in return (fromDictionary =<< fromVariant rdata)
    -- Type definition is necessary, won't compile otherwise
    case (mprops :: Maybe (M.Map String Variant)) of
        Nothing -> do  -- Request failed, trying to use cache
            dev <- getCachedDeviceInfo obj
            case dev of
                -- Device isn't in the cache, which is strange
                -- Log it and return some default values
                Nothing -> do
                    logDeviceInfoError obj
                    return DInfo { diObjectPath = B.unpack $ strObjectPath obj
                                 , diDeviceFile = "<unknown>"
                                 , diMountPoint = Just "<unknown>"
                                 , diLabel = "<unknown>"
                                 , diInternal = False
                                 , diFSystem = False
                                 }
                -- Ok, delete device from cache, because the only reason to cache access
                -- is device removal
                -- Return device info then
                Just d -> do
                    atomically $ do
                        devs <- readTVar (uDevices ?st)
                        writeTVar (uDevices ?st) $ M.delete (B.unpack $ strObjectPath obj) devs
                    return d

        -- Request successful, using it
        Just props -> do
            -- Retrieve needed properties
            let dobjpath  = B.unpack $ strObjectPath obj  -- Object path is known
            let ddevfile  = M.lookup "DeviceFile" props
            let dmounted  = M.lookup "DeviceIsMounted" props
            let dmpaths   = M.lookup "DeviceMountPaths" props
            let dlabel    = M.lookup "IdLabel" props
            let dinternal = M.lookup "DeviceIsSystemInternal" props
            let dusage    = M.lookup "IdUsage" props
            -- Build DeviceInfo structure
            let dinfo =  DInfo { diObjectPath = dobjpath
                               , diDeviceFile = fromJust $
                                                maybe (Just "<unknown>") fromVariant ddevfile
                               , diMountPoint = if fromJust $ fromVariant =<< dmounted
                                                then dmpaths >>=
                                                     fromVariant >>=
                                                     fromArray >>=
                                                     return . head
                                                else Nothing
                               , diLabel = fromJust $ maybe (Just "<unknown>") fromVariant dlabel
                               , diInternal = fromJust $ maybe (Just False) fromVariant dinternal
                               , diFSystem = ("filesystem" :: String) ==
                                             (fromJust $ maybe (Just "") fromVariant dusage)
                               }
            atomically $ do  -- Save device info to cache
                devs <- readTVar (uDevices ?st)
                writeTVar (uDevices ?st) $ M.insert dobjpath dinfo devs
            -- Return data
            return dinfo

-- Get cached device information; this may fail
getCachedDeviceInfo :: (?st :: UState) => ObjectPath -> IO (Maybe DeviceInfo)
getCachedDeviceInfo obj = do
    devs <- atomically $ readTVar (uDevices ?st)  -- Get cache map
    return $ M.lookup (B.unpack $ strObjectPath obj) devs

-- Performs substring replace to transfer device information to shell commands/notifications
substituteParameters :: (?st :: UState) => DeviceInfo -> String -> String
substituteParameters dev = replace "$DEVICE$" (diDeviceFile dev) .
                           maybe (replace "$MOUNTPATH$" "<not mounted>")
                                 (replace "$MOUNTPATH$") (diMountPoint dev) .
                           replace "$LABEL$" (diLabel dev) .
                           replace "$OBJECTPATH$" (diObjectPath dev)

