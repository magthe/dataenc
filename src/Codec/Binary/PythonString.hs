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
    ( EncIncData(..)
    , EncIncRes(..)
    , encodeInc
    , encode
    , DecIncData(..)
    , DecIncRes(..)
    , decodeInc
    , decode
    , chop
    , unchop
    ) where

import Codec.Binary.Util

import Data.Char
import Data.Maybe
import Data.Word

-- {{{1 encode
data EncIncData = EChunk [Word8] | EDone
data EncIncRes = EPart String (EncIncData -> EncIncRes) | EFinal String

encodeInc e = eI e
    where
        enc [] = []
        enc (o:os)
            | o < 0x20 || o > 0x7e = ('\\' : 'x' : toHex o) ++ enc os
            | o == 34 = "\\\"" ++ enc os
            | o == 39 = "\\'" ++ enc os
            | o == 92 = "\\\\" ++ enc os
            | otherwise = chr (fromIntegral o) : enc os

        eI EDone = EFinal []
        eI (EChunk bs) = EPart (enc bs) encodeInc

-- | Encode data.
encode :: [Word8]
    -> String
encode bs = case encodeInc (EChunk bs) of
    EPart r1 f -> case f EDone of
        EFinal r2 -> r1 ++ r2

-- {{{1 decode
decodeInc :: DecIncData String -> DecIncRes String
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
decode = decoder decodeInc

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
