-- |
-- Module    : Codec.Binary.PythonString
-- Copyright : (c) 2009 Magnus Therning
-- License   : BSD3
--
-- Implementation of python escaping.
--
-- This implementation encodes non-printable characters (0x00-0x1f, 0x7f-0xff)
-- to hex-value characters ('\xhh') while leaving printable characters as such:
--
-- @
-- \> encode [0, 10, 13, 110]
-- \"\\\\x00\\\\x0A\\\\x0Dn\"
-- \> putStrLn $ encode [0, 10, 13, 110]
-- \\x00\\x0A\\x0Dn
-- @
--
-- It also properly handles escaping of a few characters that require it:
--
-- @
-- \> encode [34, 39, 92]
-- \"\\\\\\\"\\\\\'\\\\\\\\\"
-- putStrLn $ encode [34, 39, 92]
-- \\\"\\'\\\\
-- @
--
-- Further documentation and information can be found at
-- <http://www.haskell.org/haskellwiki/Library/Data_encoding>.
module Codec.Binary.PythonString
    ( encode
    , DecIncData(..)
    , DecIncRes(..)
    , decodeInc
    , decode
    , chop
    , unchop
    ) where

import Data.Char
import Data.Word
import Data.Maybe

import Codec.Binary.Util

-- {{{1 encode
-- | Encode data.
encode :: [Word8]
    -> String
encode [] = ""
encode (o : os)
    | o < 0x20 || o > 0x7e = ('\\' : 'x' : toHex o) ++ encode os
    | o == 34 = "\\\"" ++ encode os
    | o == 39 = "\\'" ++ encode os
    | o == 92 = "\\\\" ++ encode os
    | otherwise = chr (fromIntegral o) : encode os

-- {{{1 decode
data DecIncData = Chunk String | Done
data DecIncRes = Part [Word8] (DecIncData -> DecIncRes) | Final [Word8] String | Fail [Word8] String

decodeInc :: DecIncData -> DecIncRes
decodeInc d = dI [] d
    where
        dI [] Done = Final [] []
        dI lo Done = Fail [] lo
        dI lo (Chunk s) = doDec [] (lo ++ s)
            where
                doDec acc [] = Part acc (dI [])
                doDec acc s'@('\\':'x':c0:c1:cs) = let
                        o = fromHex [c0, c1]
                    in if isJust o
                        then doDec (acc ++ [fromJust o]) cs
                        else Fail acc s'
                doDec acc s'@('\\':'\\':cs) = doDec (acc ++ [fromIntegral $ ord '\\']) cs
                doDec acc s'@('\\':'\'':cs) = doDec (acc ++ [fromIntegral $ ord '\'']) cs
                doDec acc s'@('\\':'\"':cs) = doDec (acc ++ [fromIntegral $ ord '\"']) cs
                doDec acc s'@(c:cs)
                    | c /= '\\' = doDec (acc ++ [fromIntegral $ ord c]) cs
                    | otherwise = Part acc (dI s')

-- | Decode data (strict).
decode :: String
    -> Maybe [Word8]
decode s = let
        d = decodeInc (Chunk s)
    in case d of
        Final da _ -> Just da
        Fail _ _ -> Nothing
        Part da f -> let
                d' = f Done
            in case d' of
                Final da' _ -> Just $ da ++ da'
                Fail _ _ -> Nothing
                Part _ _ -> Nothing -- should never happen

-- {{{1 chop
-- | Chop up a string in parts.
chop :: Int     -- ^ length of individual lines (values @\< 1@ are ignored)
    -> String
    -> [String]
chop n = let
        _n = max 1 n
        _chop [] = []
        _chop cs = take _n cs : _chop (drop _n cs)
    in _chop

-- {{{1 unchop
-- | Concatenate the list of strings into one long string.
unchop :: [String]
    -> String
unchop = foldr (++) ""
