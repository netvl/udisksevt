-- udisksevt source file
-- Copyright (C) Vladimir Matveev, 2010
-- Disk structure related functions
module UDisksEvt.Disk where

import Control.Monad
import Control.Concurrent.STM
import DBus.Types
import Data.Maybe

import qualified Data.Map as M
import qualified Data.Text.Lazy as B

import UDisksEvt.Datatypes

-- Cast ObjectPath value to string
objectPathToString :: ObjectPath -> String
objectPathToString = B.unpack . strObjectPath

-- Get device property from cache
getDevicePropertyCached :: (?st :: UState) => ObjectPath -> String -> IO (Maybe Variant)
getDevicePropertyCached obj pname = do
    dmap <- getDeviceInfoCached obj
    return $ M.lookup pname . diProperties =<< dmap

-- Get the whole device property Map
getDevicePropertyMapCached :: (?st :: UState) => ObjectPath -> IO (Maybe (M.Map String Variant))
getDevicePropertyMapCached obj = do
    dmap <- getDeviceInfoCached obj
    return $ fmap diProperties dmap

-- Get device info structure from runtime state
getDeviceInfoCached :: (?st :: UState) => ObjectPath -> IO (Maybe DeviceInfo)
getDeviceInfoCached obj =
    atomically $ fmap (M.lookup $ objectPathToString obj) $ readTVar (uDevices ?st)

-- Retrieve the first of device mount points
getDeviceMountPoint :: DeviceInfo -> Maybe String
getDeviceMountPoint di = M.lookup "DeviceMountPaths" (diProperties di) >>=
                         fromVariant >>= fromArray >>= listToMaybe
