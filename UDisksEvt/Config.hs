-- udisksevt source file
-- Copyright (C) DPX-Infinity, 2010
-- Configuration module

module UDisksEvt.Config (
    readConfiguration
    ) where

import Control.Monad.State
import qualified Data.Map as M
import Text.Parsec hiding (spaces, State)
import Text.Parsec.Combinator
import Text.Parsec.String
import System.IO

import UDisksEvt.Datatypes

defaultConfig :: Configuration
defaultConfig = C { cVars = M.fromList [ ("start-udisks-daemon", CVBool True)
                                       , ("shell-command", CVString "/bin/bash")
                                       ]
                  , cTriggers = M.empty
                  } 

-- Reads and parses configuration from specified file
readConfiguration :: FilePath -> IO (Maybe Configuration)
readConfiguration fname = do
    fdata <- readFile fname
    case parse fileData fname fdata of
        Left e -> do
            hPutStrLn stderr $ "Error parsing config file " ++ fname ++ ": " ++ show e
            return Nothing
        Right c -> return (Just c)

-- Converts list of configuration lines to Configuration structure
convertStructure :: [ConfigLine] -> Configuration
convertStructure lst = execState (merger lst "") defaultConfig
    where
        merger :: [ConfigLine] -> String -> State Configuration ()
        merger [] _ = return ()
        merger (x:xs) ltn = do
            trigname <- case x of
                CLVar cvn cvv -> modify (addVariable cvn cvv) >> return ltn
                CLTrigger ctn -> modify (addTrigger ctn) >> return ctn
                CLTriggerAction cta -> modify (modifyTrigger ltn cta) >> return ltn
                _ -> return ltn
            merger xs trigname

        addVariable :: String -> ConfigVarValue -> Configuration -> Configuration
        addVariable cvn cvv c@(C { cVars = cvars }) =
            c { cVars = M.insert cvn cvv cvars }        
        
        addTrigger :: String -> Configuration -> Configuration
        addTrigger ctn c@(C { cTriggers = cts }) = c { cTriggers = M.insert ctn [] cts }
        
        modifyTrigger :: String -> ConfigTriggerAction -> Configuration -> Configuration
        modifyTrigger ltn cta c@(C { cTriggers = cts }) = c { cTriggers = ncts }
            where
                ncts = M.insertWith (flip (++)) ltn [cta] cts

-- Main parser - while as whole
fileData :: Parser Configuration
fileData = (many spaces >> (commentLine <|> fileLine <|> emptyLine)) `sepEndBy` newline >>=
 --          (\x -> return $ unsafePerformIO (print x >> return x)) >>=
           return . convertStructure

-- Empty line - just 'id', because spaces already taken into account
emptyLine :: Parser ConfigLine
emptyLine = return CLEmpty

-- Comment line
commentLine :: Parser ConfigLine
commentLine = do
    char '#'
    many (noneOf "\r\n")
    return CLComment

-- File line with data
fileLine :: Parser ConfigLine
fileLine = do
    fline <- configVar <|>
             configTrigger <|>
             configTriggerActionShellCommand <|>
             configTriggerActionNotification
    many spaces
    return fline

-- Configuration variable
configVar :: Parser ConfigLine
configVar = do
    string "set"
    many spaces
    cvname <- many1 (alphaNum <|> char '-')
    many spaces
    char '='
    many spaces
    cvvalue <- configVarString <|> configVarInt <|> configVarBool
    return (CLVar cvname cvvalue)

-- Configuration trigger
configTrigger :: Parser ConfigLine
configTrigger = do
    string "on"
    many spaces
    ctname <- many1 letter
    many spaces
    char ':'
    return (CLTrigger ctname)

-- Config trigger action - shell command
configTriggerActionShellCommand :: Parser ConfigLine
configTriggerActionShellCommand = do
    string "run"
    many spaces
    cmd <- many1 (noneOf "\r\n")
    return (CLTriggerAction (CTAShellCommand cmd))

-- Config trigger action - notification
configTriggerActionNotification :: Parser ConfigLine
configTriggerActionNotification = do
    string "notify"
    many spaces
    nbody <- quoted mstring1
    -- This complicated structure parses 4 optional parameters of different types
    -- I have a feeling that this can be done more easily
    (nsummary, nicon, ntimeout, nurgency) <- do
        many spaces
        nsummary' <- optionMaybe (quoted mstring)
        case nsummary' of
            Nothing -> return ("", "", 0, NUNormal)
            Just nsummary -> do
                many spaces
                nicon' <- optionMaybe (quoted mstring)
                case nicon' of
                    Nothing -> return (nsummary, "", 0, NUNormal)
                    Just nicon -> do
                        many spaces
                        ntimeout' <- optionMaybe number
                        case ntimeout' of
                            Nothing -> return (nsummary, nicon, 0, NUNormal)
                            Just ntimeout -> do
                                many spaces
                                nurgency' <- optionMaybe (string "low" <|>
                                                          string "normal" <|>
                                                          string "critical")
                                case nurgency' of
                                    Nothing -> return (nsummary, nicon, ntimeout, NUNormal)
                                    Just nurgency -> return (nsummary, nicon, ntimeout, NUNormal)
    return (CLTriggerAction (CTANotification nbody nsummary nicon ntimeout nurgency))

-- Config variable string value
configVarString :: Parser ConfigVarValue
configVarString = quoted mstring >>= return . CVString

-- Config variable positive integer value
configVarInt :: Parser ConfigVarValue
configVarInt = number >>= return . CVInt

-- Config variable boolean value
configVarBool = (string "yes" <|> string "no") >>= return . CVBool . readBool
    where
        readBool "yes" = True
        readBool "no"  = False
        readBool _     = undefined

-- Quoted something
quoted :: Parser a -> Parser a
quoted = between (char '"') (char '"')

-- String that can be used in quotes, possibly empty
mstring :: Parser String
mstring = many (noneOf "\"\r\n")

-- Non-empty string
mstring1 :: Parser String
mstring1 = many1 (noneOf "\"\r\n")

-- Just sequence of digits
number :: Parser Int
number = many1 digit >>= return . read

-- Redefinition of the same parser in library,
-- because we don't need newline and such thing as spaces
spaces :: Parser Char
spaces = oneOf " \t"
