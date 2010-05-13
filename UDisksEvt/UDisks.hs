-- udisksevt source file
-- Copyright (C) DPX-Infinity, 2010
-- Module for communicating with UDisks through D-Bus

module UDisksEvt.UDisks where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.Map as M
import qualified Data.Text.Lazy as B
import Data.List
import Data.Maybe
import DBus.Bus
import DBus.Client
import DBus.MatchRule as MR
import DBus.Message
import DBus.Types

import UDisksEvt.Config
import UDisksEvt.Datatypes

triggers = [ ("added", MatchRule { matchType = Just MR.Signal
                                 , matchSender = Just "org.freedesktop.UDisks"
                                 , matchDestination = Nothing
                                 , matchPath = Just "/org/freedesktop/UDisks"
                                 , matchInterface = Just "org.freedesktop.UDisks"
                                 , matchMember = Just "DeviceAdded"
                                 , matchParameters = []
                                 })
           , ("removed", MatchRule { matchType = Just MR.Signal
                                   , matchSender = Just "org.freedesktop.UDisks"
                                   , matchDestination = Nothing
                                   , matchPath = Just "/org/freedesktop/UDisks"
                                   , matchInterface = Just "org.freedesktop.UDisks"
                                   , matchMember = Just "DeviceRemoved"
                                   , matchParameters = []
                                   })
           , ("mounted", MatchRule { matchType = Just MR.Signal
                                   , matchSender = Just "org.freedesktop.UDisks"
                                   , matchDestination = Nothing
                                   , matchPath = Just "/org/freedesktop/UDisks"
                                   , matchInterface = Just "org.freedesktop.UDisks"
                                   , matchMember = Just "DeviceJobChanged"
                                   , matchParameters =
                                       [StringValue 3 "FilesystemMount"]
                                   })
           , ("unmounted", MatchRule { matchType = Just MR.Signal
                                     , matchSender = Just "org.freedesktop.UDisks"
                                     , matchDestination = Nothing
                                     , matchPath = Just "/org/freedesktop/UDisks"
                                     , matchInterface = Just "org.freedesktop.UDisks"
                                     , matchMember = Just "DeviceJobChanged"
                                     , matchParameters =
                                         [StringValue 3 "FilesystemUnmount"]
                                     })
           ]
           
udisksProxy = Proxy (RemoteObject "org.freedesktop.UDisks" "/org/freedesktop/UDisks")
              "org.freedesktop.UDisks"

deviceProxy dev = Proxy (RemoteObject "org.freedesktop.UDisks" dev)
                  "org.freedesktop.UDisks.Device"

devicePropertyProxy dev = Proxy (RemoteObject "org.freedesktop.UDisks" dev)
                         "org.freedesktop.DBus.Properties"

systemBusClient = mkClient =<< getSystemBus

logDBusError = undefined

logDaemonStartup = undefined

logTriggerError = undefined

logDeviceInfoError = undefined

runShell = undefined

showNotification = undefined

-- Checks if device is system internal
isDeviceInternal :: ObjectPath -> IO Bool
isDeviceInternal obj = do
    client <- systemBusClient
    response <- callProxyBlocking client (devicePropertyProxy obj) "Get" [] $
        map toVariant (["org.freedesktop.UDisks.Device", "DeviceIsSystemInternal"] :: [String])
    case response of
        Left _ -> return False
        Right resp ->
            let Just isinternal = fromVariant $ head $ messageBody resp
            in return isinternal
        
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
    let ?st = U conf devs
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
    -- Exit if device is internal - we do not have to act on such devices
    isDeviceInternal obj >>= (flip when) (return ())
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
                                 }
                -- Ok, returning data from cache
                Just d -> return d
        -- Request successful, using in
        Just props -> do
            -- Retrieve needed properties
            let dobjpath = B.unpack $ strObjectPath obj  -- Object path is known
            let ddevfile = M.lookup "DeviceFile" props
            let dmounted = M.lookup "DeviceIsMounted" props
            let dmpaths  = M.lookup "DeviceMountPaths" props
            let dlabel   = M.lookup "IdLabel" props
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
                               }
            -- TODO: Update cache
            -- Return data
            return dinfo

-- Get cached device information; this may fail
getCachedDeviceInfo :: (?st :: UState) => ObjectPath -> IO (Maybe DeviceInfo)
getCachedDeviceInfo obj = do
    devs <- atomically $ readTVar $ uDevices ?st  -- Get cache map
    return $ M.lookup (B.unpack $ strObjectPath obj) devs

substituteParameters :: (?st :: UState) => DeviceInfo -> String -> String
substituteParameters dev str = undefined
    