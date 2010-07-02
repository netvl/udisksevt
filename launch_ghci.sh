#!/bin/bash

ghci -XImplicitParams -XOverloadedStrings -hide-package monads-tf Main.hs ./UDisksEvt/{UDisks,Config,Disk,DBus,Datatypes}.hs
