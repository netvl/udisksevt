-- udisksevt source file
-- Copyright (C) DPX-Infinity, 2010
-- Datatypes collection module

module UDisksEvt.Datatypes where

import Data.Map

-- Main configuration datatype
data Configuration = C { cVars :: Map String ConfigVarValue
                       , cTriggers :: Map String [ConfigTriggerAction]
                       }
                   deriving (Show)

-- Configuration variable value
data ConfigVarValue = CVString String
                    | CVBool Bool
                    | CVInt Int
                    deriving (Show)

-- Configuration trigger action - these commands definition
data ConfigTriggerAction = CTAShellCommand { ctascCommand :: String
                                           }
                         | CTANotification { ctanBody :: String
                                           , ctanSummary :: String
                                           , ctanIcon :: String
                                           , ctanTimeout :: Int	
                                           , ctanUrgency :: NotificationUrgency
                                           }
                         deriving (Show)

-- Notification urgency parameter
data NotificationUrgency = NULow                         
                         | NUNormal
                         | NUCritical
                         deriving (Show)

-- Intermediate structure used in config parsing
data ConfigLine = CLVar String ConfigVarValue
                | CLTrigger String
                | CLTriggerAction ConfigTriggerAction
                | CLEmpty
                | CLComment
                deriving (Show)
