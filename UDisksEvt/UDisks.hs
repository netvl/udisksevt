-- udisksevt source file
-- Copyright (C) DPX-Infinity, 2010
-- Module for communicating with UDisks through D-Bus

module UDisksEvt.UDisks where

import qualified Data.Map as M
import Data.Maybe
import Data.Text.Lazy hiding (head, map)
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

runSignalHandlers :: Configuration -> IO ()
runSignalHandlers conf = do
    client <- systemBusClient
    let ?conf = conf
    mapM_ (setSignal client) triggers
    where
        setSignal :: Client -> (String, MatchRule) -> IO ()
        setSignal client (rtype, rule) = onSignal client rule (signalDispatcher conf rtype)

signalDispatcher :: (?conf :: Configuration) => String -> BusName -> Signal -> IO ()
signalDispatcher conf rtype bname sig = runTrigger rtype (fromVariant $ head $ signalBody sig)

runTrigger :: (?conf :: Configuration) => String -> ObjectPath -> IO ()
runTrigger rtype obj = do
    let mctas = getTrigger rtype
    case mctas of
        Nothing -> logTriggerError rtype
        Just ctas -> mapM_ (executeTriggerAction obj) ctas

executeTriggerAction :: (?conf :: Configuration) => ObjectPath -> ConfigTriggerAction -> IO ()
executeTriggerAction obj cta =
    case cta of
        CTAShellCommand sc -> runShell $ substituteParameters obj sc
        CTANotification nbody nsummary nicon ntimeout nurgency ->
            showNotification (substituteParameters obj nbody) (substituteParameters obj nsummary)
                nicon ntimeout nurgency
    