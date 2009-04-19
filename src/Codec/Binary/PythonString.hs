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
    , decode
    , decode'
    , chop
    , unchop
    ) where

import Data.Char
import Data.Word

import Codec.Binary.Util

-- {{{1 encode
-- | Encode data.
encode :: [Word8]
    -> String
encode [] = ""
encode (o:os)
    | o < 0x20 || o > 0x7e = ('\\' : 'x' : toHex o) ++ encode os
    | o == 34 = "\\\"" ++ encode os
    | o == 39 = "\\'" ++ encode os
    | o == 92 = "\\\\" ++ encode os
    | otherwise = chr (fromIntegral o) : encode os

-- {{{1 decode
-- -- | Decode data (lazy).
decode' :: String
    -> [Maybe Word8]
decode' [] = []
decode' ('\\':'x':c0:c1:cs) = fromHex [c0, c1] : decode' cs
decode' ('\\':'\\':cs) = (Just $ fromIntegral $ ord '\\') : decode' cs
decode' ('\\':'\'':cs) = (Just $ fromIntegral $ ord '\'') : decode' cs
decode' ('\\':'\"':cs) = (Just $ fromIntegral $ ord '\"') : decode' cs
decode' (c:cs)
    | c /= '\\' = (Just $ fromIntegral $ ord c) : decode' cs
    | otherwise = [Nothing]

-- | Decode data (strict).
decode :: String -> Maybe [Word8]
decode = sequence . decode'

-- {{{1 chop
-- | Chop up a string in parts.
chop :: Int     -- ^ length of individual lines (values @\< 1@ are ignored)
    -> String
    -> [String]
chop n = let
        _n = max 1 n
        _chop [] = []
        _chop cs = take _n cs : (_chop $ drop _n cs)
    in _chop

-- {{{1 unchop
-- | Concatenate the list of strings into one long string.
unchop :: [String]
    -> String
unchop = foldr (++) ""
