-- udisksevt source file
-- Copyright (C) Vladimir Matveev, 2010
-- Various utility functions
module UDisksEvt.Utils where

import DBus.Types
import Data.Int
import Data.List
import Data.Maybe
import Data.Word

import qualified Data.Text.Lazy as B

-- Perform a substring replace
-- http://bluebones.net/2007/01/replace-in-haskell/ - Joseph's function
replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace [] _ list = list
replace oldSub newSub list = _replace list where
	_replace list@(h:ts) = if oldSub `isPrefixOf` list
		then newSub ++ _replace (drop len list)
		else h : _replace ts
	_replace [] = []
	len = length oldSub

-- Fork morphism from pair calculus
fork :: (a -> b) -> (a -> c) -> a -> (b, c)
fork f g x = (f x, g x)

-- Cast ObjectPath value to string
objectPathToString :: ObjectPath -> String
objectPathToString = B.unpack . strObjectPath

-- Converts arbitrary Variant value to its string representation
-- Just shows plain values, recursively shows Variant values,
-- shows special values as strings and takes the first element of
-- dictionaries and arrays
variantToString :: Variant -> String
variantToString v = f (variantType v)
    where  -- These are different cases of Variant types
        f DBusString =
            fromJust $ (fromVariant v :: Maybe String)
        f DBusObjectPath =
            objectPathToString $ fromJust $ (fromVariant v :: Maybe ObjectPath)
        f DBusSignature =
            B.unpack $ strSignature $ fromJust $ (fromVariant v :: Maybe Signature)
        f DBusVariant =
            variantToString $ fromJust $ (fromVariant v :: Maybe Variant)
        f (DBusArray _) =
            maybe "" variantToString $ listToMaybe $
            arrayItems $ fromJust $ fromVariant $ v
        f (DBusDictionary _ _) =
            maybe "" variantToString $ fmap snd $ listToMaybe $
            dictionaryItems $ fromJust $ fromVariant v
        f t = case t of
            DBusBoolean -> show $ fromJust $ (fromVariant v :: Maybe Bool)
            DBusByte    -> show $ fromJust $ (fromVariant v :: Maybe Word8)
            DBusInt16   -> show $ fromJust $ (fromVariant v :: Maybe Int16)
            DBusInt32   -> show $ fromJust $ (fromVariant v :: Maybe Int32)
            DBusInt64   -> show $ fromJust $ (fromVariant v :: Maybe Int64)
            DBusWord16	-> show $ fromJust $ (fromVariant v :: Maybe Word16)
            DBusWord32	-> show $ fromJust $ (fromVariant v :: Maybe Word32)
            DBusWord64	-> show $ fromJust $ (fromVariant v :: Maybe Word64)
            DBusDouble	-> show $ fromJust $ (fromVariant v :: Maybe Double)

    {- replace "$DEVICE$" (diDeviceFile dev) .
    maybe (replace "$MOUNTPATH$" "<not mounted>")                           
    (replace "$MOUNTPATH$") (diMountPoint dev) .
    replace "$LABEL$" (diLabel dev) .
    replace "$OBJECTPATH$" (diObjectPath dev) -}
