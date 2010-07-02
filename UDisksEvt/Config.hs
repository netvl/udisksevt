-- udisksevt source file
-- Copyright (C) Vladimir Matveev, 2010
-- Configuration module

module UDisksEvt.Config ( readConfiguration
                        , getVariable
                        , getTrigger
                        ) where

import Control.Monad
import Data.Maybe
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Data.Map as M

import UDisksEvt.Datatypes
import UDisksEvt.Log

-- Default configuration
defaultConfig :: Configuration
defaultConfig = C { cVars = M.fromList [ ("start-udisks-daemon", CVBool True)
                                       , ("shell-command", CVString "/bin/bash")
                                       , ("default-notification-icon", CVString "")
                                       ]
                  , cTriggers = M.empty
                  }

-- Extract variable from Configuration
getVariable :: Configuration -> String -> Maybe ConfigVarValue
getVariable conf cvn = M.lookup cvn (cVars conf)

-- Extract list of trigger actions from Configuration
getTrigger :: Configuration -> String -> Maybe [ConfigTriggerAction]
getTrigger conf ctn = M.lookup ctn (cTriggers conf)

-- Reads and parses configuration from specified file
readConfiguration :: FilePath -> IO Configuration
readConfiguration fname = do
    fdata <- readFile fname
    case runParser fileData (CPS Nothing defaultConfig) fname fdata of
        Left e -> do
            logError $ "Error parsing config file:\n" ++ show e
            logError $ "Dropping to default values."
            return defaultConfig
        Right conf -> return conf
    

-- Main parser - while as whole
fileData :: ConfParser Configuration
fileData = (many spaces >> (commentLine <|> fileLine <|> emptyLine)) `sepEndBy` newline >>
           getState >>= return . cpsConfiguration

-- Empty line - just 'id', because spaces already taken into account
emptyLine :: ConfParser ()
emptyLine = return ()

-- Comment line
commentLine :: ConfParser ()
commentLine = char '#' >> many (noneOf "\r\n") >> return ()

-- File line with data
fileLine :: ConfParser ()
fileLine = (configVar <|>
            configTrigger <|>
            configTriggerActionShellCommand <|>
            configTriggerActionNotification) >>
           many spaces >> return ()

-- Configuration variable
configVar :: ConfParser ()
configVar = do
    string "set" >> many1 spaces
    cvname <- many1 (alphaNum <|> char '-')
    many spaces >> char '=' >> many spaces
    cvvalue <- configVarString <|> configVarInt <|> configVarBool
    setVariable cvname cvvalue

-- Configuration trigger
configTrigger :: ConfParser ()
configTrigger = do
    string "on" >> many1 spaces
    ctname <- many1 letter
    many spaces >> char ':'
    beginTrigger ctname

-- Config trigger action - shell command
configTriggerActionShellCommand :: ConfParser ()
configTriggerActionShellCommand = do
    string "run" >> many1 spaces
    cmd <- many1 (noneOf "\r\n")
    insertTriggerAction "run" (CTAShellCommand cmd)

-- Config trigger action - notification
configTriggerActionNotification :: ConfParser ()
configTriggerActionNotification = do
    string "notify" >> many1 spaces
    nbody <- quoted mstring1
    -- This complicated structure parses 4 optional parameters of different types
    -- I have a feeling that this can be done more easily
    (nsummary, nicon, ntimeout, nurgency) <- do
        many spaces
        nsummary' <- optionMaybe (quoted mstring)
        case nsummary' of
            Nothing -> return ("", "default", -1, NUNormal)
            Just nsummary -> do
                many spaces
                nicon' <- optionMaybe (quoted mstring)
                case nicon' of
                    Nothing -> return (nsummary, "default", -1, NUNormal)
                    Just nicon -> do
                        many spaces
                        ntimeout' <- optionMaybe number
                        case ntimeout' of
                            Nothing -> return (nsummary, nicon, -1, NUNormal)
                            Just ntimeout -> do
                                many spaces
                                nurgency' <- optionMaybe (string "low" <|>
                                                          string "normal" <|>
                                                          string "critical")
                                case nurgency' of
                                    Nothing -> return (nsummary, nicon, ntimeout, NUNormal)
                                    Just nurgency -> return (nsummary, nicon, ntimeout, read nurgency)
    insertTriggerAction "notify" (CTANotification nbody nsummary nicon ntimeout nurgency)

-- Set variable in parser state
setVariable :: String -> ConfigVarValue -> ConfParser ()
setVariable cvname cvvalue =
    updateState $ \st@(CPS { cpsConfiguration = conf }) ->
    st { cpsConfiguration = conf { cVars = M.insert cvname cvvalue (cVars conf) } }

-- Begin new trigger section
beginTrigger :: String -> ConfParser ()
beginTrigger ctname =
    updateState $ \st -> st { cpsCurrentTrigger = Just ctname }

-- Insert trigger action into parsing state
insertTriggerAction :: String -> ConfigTriggerAction -> ConfParser ()
insertTriggerAction tn cta = do
    st <- getState
    when (isNothing $ cpsCurrentTrigger st) $
        fail ("unexpected " ++ tn ++ " action")
    let conf = cpsConfiguration st
        Just ct = cpsCurrentTrigger st
    setState $
        st { cpsConfiguration = conf { cTriggers = M.insertWith (flip (++))
                                                   ct
                                                   [cta]
                                                   (cTriggers conf) } }

-- Config variable string value
configVarString :: ConfParser ConfigVarValue
configVarString = fmap CVString (quoted mstring)

-- Config variable integer value
configVarInt :: ConfParser ConfigVarValue
configVarInt = fmap CVInt (signed number)

-- Config variable boolean value
configVarBool :: ConfParser ConfigVarValue
configVarBool = fmap (CVBool . readBool) (string "yes" <|> string "no")
    where
        readBool "yes" = True
        readBool "no"  = False
        readBool _     = undefined

-- Quoted something
quoted :: ConfParser a -> ConfParser a
quoted = between (char '"') (char '"')

-- String that can be used in quotes, possibly empty
mstring :: ConfParser String
mstring = many (noneOf "\"\r\n")

-- Non-empty string
mstring1 :: ConfParser String
mstring1 = many1 (noneOf "\"\r\n")

-- Just sequence of digits
number :: ConfParser Int
number = fmap read (many1 digit)

-- Signed integer
signed :: ConfParser Int -> ConfParser Int
signed p = do
    sgn <- option '+' (char '-' <|> char '+')
    case sgn of
        '+' -> p
        '-' -> fmap negate p

-- Redefinition of the same parser in library;
-- we don't need newline and such thing as spaces
spaces :: ConfParser Char
spaces = oneOf " \t"
