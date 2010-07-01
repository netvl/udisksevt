-- udisksevt source file
-- Copyright (C) Vladimir Matveev, 2010m
-- D-Bus interface to UDisks daemon
module UDisksEvt.DBus where

import Control.Monad
import DBus.Bus
import DBus.Client
import DBus.Message
import DBus.Types
import Data.List
import Data.Int
import Data.Maybe
import Data.Word
import System.Environment

import qualified Data.Map as M

import UDisksEvt.Datatypes
import UDisksEvt.Disk
import UDisksEvt.Log

--- MOVE SOMEHERE ---
-- http://bluebones.net/2007/01/replace-in-haskell/ - Joseph's function
replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace [] _ list = list
replace oldSub newSub list = _replace list where
	_replace list@(h:ts) = if oldSub `isPrefixOf` list
		then newSub ++ _replace (drop len list)
		else h : _replace ts
	_replace [] = []
	len = length oldSub
--- MOVE SOMEWHERE ---

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

-- Wake UDisks daemon with a request
wakeDaemon :: IO ()
wakeDaemon = do
    client <- systemBusClient
    callProxy client udisksProxy "EnumerateDevices" [] [] logDBusError logDaemonStartup

-- Show notification using D-Bus org.freedesktop.Notifications server, if present
showNotification :: (?st :: UState) => String -> String -> String -> Int -> NotificationUrgency -> IO ()
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
    callProxy client notificationProxy "Notify" []
        ([ toVariant ("UDisksEvt" :: String)
         , toVariant (0 :: Word32)
         , toVariant ricon'
         , toVariant summary
         , toVariant body
         , toVariant actions
         , toVariant hints
         , toVariant (fromIntegral timeout :: Int32)
         ])
        (logNotifyError summary body) (logNotifyOk summary body ricon')
    return ()

-- Get device property either from UDisks or from device cache
getDeviceProperty :: (?st :: UState) => ObjectPath -> String -> IO (Maybe Variant)
getDeviceProperty obj pname = do
    client <- systemBusClient
    response <- callProxyBlocking client (devicePropertyProxy obj) "Get" [] $
                map toVariant ["org.freedesktop.UDisks.Device", pname]
    case response of
        Left _ -> do  -- Error, trying to retrieve value from cache
            prop <- getDevicePropertyCached obj pname
            case prop of
                Nothing -> do
                    logError $ "Unable to retrieve property " ++ pname ++
                        "of device " ++ show obj
                    return Nothing
                jp -> return jp
        -- Ok, get response head, it's the value
        Right r -> return $ listToMaybe $ messageBody r

-- Retrieve all device properties as a Map either from UDisks or from cache
-- Returns the device properties map and a boolean flag showing whether
-- the cache was used
getDevicePropertyMap :: (?st :: UState) => ObjectPath -> IO (Bool, Maybe (M.Map String Variant))
getDevicePropertyMap obj = do
    client <- systemBusClient
    response <- callProxyBlocking client (devicePropertyProxy obj) "GetAll" [] $
                [toVariant ("org.freedesktop.UDisks.Device" :: String)]
    case response of
        Left _ -> do  -- Error, trying to get cached map
            mpm <- getDevicePropertyMapCached obj :: IO (Maybe (M.Map String Variant))
            return (True, mpm)
        Right r -> do  -- Ok, extracting map from D-Bus dictionary
            let rh = head $ messageBody r
                Just d = fromVariant rh
                mpm = fromDictionary d -- :: Maybe (Map String Variant)
            return (False, mpm)

-- Retrieve some device properties using it's property map in order to
-- avoid a D-Bus access overhead
-- Returns an empty list if some error occured
getDeviceProperties :: (?st :: UState) => ObjectPath -> [String] -> IO [Maybe Variant]
getDeviceProperties obj pnames = do
    (_, mpm) <- getDevicePropertyMap obj
    return $ case mpm of
        Nothing -> []
        Just pm -> map (flip M.lookup pm) pnames

-- Checks if device is system internal
-- It is so if DeviceIsSystemInternal property is true
isDeviceInternal :: (?st :: UState) => ObjectPath -> IO Bool
isDeviceInternal obj = do
    v <- getDeviceProperty obj "DeviceIsSystemInternal"
    return $ case fromVariant =<< v of
        Nothing -> False
        Just isinternal -> isinternal

-- Checks is device is mountable filesystem
-- It is so if IdUsage property is "filesystem"
isDeviceFilesystem :: (?st :: UState) => ObjectPath -> IO Bool
isDeviceFilesystem obj = do
    v <- getDeviceProperty obj "IdUsage"
    return $ case fromVariant =<< v of
        Nothing -> False
        Just devtype -> devtype == ("filesystem" :: String)
