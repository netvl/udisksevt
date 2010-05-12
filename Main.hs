-- udisksevt source file
-- Copyright (C) DPX-Infinity, 2010
-- Main module

module Main where

import UDisksEvt.Config
import UDisksEvt.Datatypes
import UDisksEvt.UDisks

main :: IO ()
main = do
    conf <- readConfiguration "udisksevt.conf"
    print conf
    return ()
