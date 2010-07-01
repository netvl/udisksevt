-- udisksevt source file
-- Copyright (C) Vladimir Matveev, 2010
-- Main module

module Main where

import qualified Data.Map as M
import Control.Concurrent (threadDelay)
import Control.Monad (when, forever)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Posix.IO
import System.Posix.Process

import UDisksEvt.Config
import UDisksEvt.Datatypes
import UDisksEvt.UDisks

data Flag = ConfigFile String  -- Configuration file argument
          | Daemonize          -- Daemonize flag
          deriving (Eq, Show)

table = [ Option ['c'] ["config"] (ReqArg ConfigFile "FILE")
          "configuration file; defaults to ~/.config/udisksevt/udisksevt.conf"
        , Option ['d'] ["daemonize"] (NoArg Daemonize) "daemonize, i.e. detach from console"
        ]

-- Default configuration location
configPath :: IO String
configPath = fmap (++ "/.config/udisksevt/udisksevt.conf") (getEnv "HOME")

-- Parse command line arguments
parseArgs :: [String] -> IO (String, Bool)
parseArgs argv = case getOpt Permute table argv of
    (opts, _, []) -> do
        let daemonize = Daemonize `elem` opts
        cpath <- configPath
        let confpath = foldr selectConfPath cpath opts
        return (confpath, daemonize)
    (_, _, errs) -> do
        hPutStrLn stderr $ concat errs ++
            usageInfo "Usage: udisksevt [-d | --daemonize] [-c FILE | --config=FILE]" table
        exitWith (ExitFailure 1)
    where
        selectConfPath :: Flag -> String -> String
        selectConfPath (ConfigFile c) _ = c
        selectConfPath _ c = c

-- Fork to background using action and do other useful things
doDaemonize :: IO () -> IO ()
doDaemonize prog = do
    forkProcess $ do
        createSession
        forkProcess $ do
            nullFd <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
            mapM_ closeFd [stdInput, stdOutput, stdError]
            mapM_ (dupTo nullFd) [stdInput, stdOutput, stdError]
            closeFd nullFd
            prog
        exitImmediately ExitSuccess
    putStrLn "Forked to background."
    exitImmediately ExitSuccess

main :: IO ()
main = do
    (confpath, daemonize) <- parseArgs =<< getArgs
    putStr "Reading configuration... "
    conf <- readConfiguration confpath
    putStrLn "Done."
    if daemonize  -- Fork to background if needed
        then doDaemonize (prog conf)
        else prog conf
        
    where
        prog :: Configuration -> IO ()  -- Main program
        prog conf = do
            -- Wake up UDisks daemon
            case M.lookup "start-udisks-daemon" (cVars conf) of
                Just (CVBool True) -> do
                    putStrLn "Starting UDisks daemon..."
                    wakeDaemon
                _ -> return ()
            putStr "Setting up D-Bus signal handlers... "
            runSignalHandlers conf  -- Set UDisks signal handlers
            putStrLn "Done."
            putStrLn "Entering endless loop."
            forever $ threadDelay 10000000   -- Loop forever
