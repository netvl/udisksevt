-- udisksevt source file
-- Copyright (C) DPX-Infinity, 2010
-- Datatypes collection module

module UDiskeEvt.Datatypes where

-- Main configuration datatype
data Configuration = C { cVars :: [ConfigVar]
                       , cTriggers :: [ConfigTrigger]
                       }
                   deriving (Show)

-- Configuration variable datatype
data ConfigVar = CVar { cvName :: String
                      , cvValue :: ConfigVarValue
                      }
               deriving (Show)

-- Configuration variable value
data ConfigVarValue = CVString String
                    | CVBool Bool
                    | CVInt Int
                    deriving (Show)

-- Configuration trigger - an event on which some commands can be run
data ConfigTrigger = CTrigger { ctName :: String
                              , ctActions :: [ConfigTriggerAction]
                              }
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
data ConfigLine = CLVar ConfigVar
                | CLTrigger String
                | CLTriggerAction ConfigTriggerAction
                | CLEmpty
                | CLComment
                deriving (Show)
