-- udisksevt source file
-- Copyright (C) DPX-Infinity, 2010
-- Main module

module Main where

--import UDisksEvt.Config
import UDisksEvt.UDisks

main :: IO ()
main = do
    dv <- enumerateDevices
    mapM_ putStrLn dv
    return ()
