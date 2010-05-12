-- udisksevt source file
-- Copyright (C) DPX-Infinity, 2010
-- Configuration module

module UDisksEvt.Config (
    readConfiguration
    ) where

import Control.Monad.State
import Text.Parsec hiding (spaces, State)
import Text.Parsec.Combinator
import Text.Parsec.String

import UDisksEvt.Datatypes

readConfiguration :: FilePath -> IO (Maybe Configuration)
readConfiguration fname = do
    fdata <- readFile fname
    case parse fileData fname fdata of
        Left e -> do
            hPutStrLn stderr $ "Error parsing config file " ++ fname ++ ": " ++ show e
            return Nothing
        Right c -> return (Just c)

mPutLine :: ConfigLine -> State (Configuration, [ConfigLine]) () 
mPutLine ln = do
    (conf, lst) <- get
    put (conf, ln:lst)

mChangeConfig :: (Configuration -> Configuration) -> State (Configuration, [ConfigLine]) ()
mChangeConfig f = do
    (conf, lst) <- get
    put (f conf, lst)

revertTriggers :: Configuration -> Configuration
revertTriggers

mergeVariables :: (Configuration, [ConfigLine]) -> (Configuration, [ConfigLine])
mergeVariables (conf, lst) = execState (merger lst) (conf, [])
    where
        merger :: [ConfigLine] -> State (Configuration, [ConfigLine]) ()
        merger [] = return ()
        merger (x:xs) = do
            case x of
                CLVar cvar -> mChangeConfig (addVariable cvar)
                _ -> mPutLine x
            merger xs

        addVariable :: ConfigVar -> Configuration -> Configuration
        addVariable cvar c@(C { cVars = cvars }) = c { cVars = cvar:cvars }

mergeTriggers :: (Configuration, [ConfigLine]) -> (Configuration, [ConfigLine])
mergeTriggers (conf, lst) = execState (merger lst) (conf, [])
    where
        merger :: [ConfigLine] -> State (Configuration, [ConfigLine]) ()
        merger [] = return ()
        merger (x:xs) = do
            case x of
                CLTrigger ctname -> mChangeConfig (insertTrigger (CTrigger ctname []))
                CLTriggerAction cta -> mChangeConfig (modifyLastTrigger cta)
                _ -> mPutLine x
            merger xs
            
        insertTrigger :: ConfigTrigger -> Configuration -> Configuration
        insertTrigger ct c@(C { cTriggers = cts }) = c { cTriggers = ct:cts }

        modifyLastTrigger :: ConfigTriggerAction -> Configuration -> Configuration
        modifyLastTrigger cta c@(C { cTriggers = ct@(CTrigger { ctActions = ctas }):cts }) =
            c { cTriggers = (ct { ctActions = cta:ctas } ):cts }

cleanLines :: [ConfigLine] -> [ConfigLine]
cleanLines = filter f
    where
        f CLEmpty = False
        f CLComment = False
        f _ = True

-- Main parser - while as whole
fileData :: Parser Configuration
fileData = (fileLine <|> emptyLine <|> commentLine) `sepEndBy` newline >>=
           return . revertTriggers . nubVariables . fst .
           mergeTriggers . mergeVariables . (C [] [],) . cleanLines

emptyLine :: Parser ConfigLine
emptyLine = many spaces >> return CLEmpty

commentLine :: Parser ConfigLine
commentLine = do
    many spaces
    char '#'
    many (noneOf "\r\n")
    return CLComment

-- File line with data
fileLine :: Parser ConfigLine
fileLine = do
    many spaces
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
    cvname <- many1 letter
    many spaces
    char '='
    many spaces
    cvvalue <- configVarString <|> configVarInt <|> configVarBool
    return (CLVar (CVar cvname cvvalue))

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


configVarString :: Parser ConfigVarValue
configVarString = quoted mstring >>= return . CVString

configVarInt :: Parser ConfigVarValue
configVarInt = number >>= return . CVInt

configVarBool = (string "yes" <|> string "no") >>= return . CVBool . readBool
    where
        readBool "yes" = True
        readBool "no"  = False
        readBool _     = undefined

quoted :: Parser a -> Parser a
quoted = between (char '"') (char '"')

mstring :: Parser String
mstring = many (noneOf "\"\r\n")

mstring1 :: Parser String
mstring1 = many1 (noneOf "\"\r\n")

number :: Parser Int
number = many1 digit >>= return . read

spaces :: Parser Char
spaces = oneOf " \t"
