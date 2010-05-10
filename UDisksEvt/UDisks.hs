-- udisksevt source file
-- Copyright (C) DPX-Infinity, 2010
-- Module for communicating with UDisks through D-Bus
-- {-# LANGUAGE: OverloadedStrings #-}

module UDisksEvt.UDisks where

import DBus.Client
import DBus.Types
import DBus.Bus
import DBus.Message
import Data.Maybe
import Data.Text.Lazy hiding (map,head)

enumerateDevices :: IO [String]
enumerateDevices = do
    client <- mkClient =<< getSystemBus
    let proxy = Proxy (RemoteObject "org.freedesktop.UDisks" "/org/freedesktop/UDisks") "org.freedesktop.UDisks"
    response <- callProxyBlocking_ client proxy "EnumerateDevices" [] []
    let rdata = head $ messageBody response
    let Just lst1 = fromArray =<< fromVariant rdata
    return $ map (unpack . strObjectPath) lst1
