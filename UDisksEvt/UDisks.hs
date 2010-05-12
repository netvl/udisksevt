-- udisksevt source file
-- Copyright (C) DPX-Infinity, 2010
-- Module for communicating with UDisks through D-Bus
-- {-# LANGUAGE: OverloadedStrings #-}

module UDisksEvt.UDisks where

import Data.Maybe
import Data.Text.Lazy hiding (head, map)
import DBus.Client
import DBus.Types
import DBus.Bus
import DBus.Message

import UDisksEvt.Config
import UDisksEvt.Datatypes

udisksProxy = Proxy (RemoteObject "org.freedesktop.UDisks" "/org/freedesktop/UDisks")
              "org.freedesktop.UDisks"

deviceProxy dev = Proxy (RemoteObject "org.freedesktop.UDisks" dev)
                  "org.freedesktop.UDisks.Device"

devicePropertyProxy dev = Proxy (RemoteObject "org.freedesktop.UDisks" dev)
                         "org.freedesktop.DBus.Properties"

systemBusClient = mkClient =<< getSystemBus

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
    return $ map (unpack . strObjectPath) lst1
