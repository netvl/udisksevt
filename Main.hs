-- udisksevt source file
-- Copyright (C) DPX-Infinity, 2010
-- Main module

module Main where

import UDisksEvt.Config
import UDisksEvt.Datatypes
import UDisksEvt.UDisks
import Control.Concurrent.STM
import qualified Data.Map as M

main :: IO ()
main = do
    Just conf <- readConfiguration "udisksevt.conf"
    devs <- newTVarIO M.empty
    let ?st = U conf devs
    showNotification "test" "test" "" 5000 NUNormal
    return ()
