-- |
-- Module    : Codec.Binary.Hexadecimal
-- Copyright : (c) 2009 Magnus Therning
-- License   : BSD3
--
-- Simple hexadecimal encoding.
--
-- Further documentation and information can be found at
-- <http://www.haskell.org/haskellwiki/Library/Data_encoding>.
module Codec.Binary.Hexadecimal
    ( encode
    , decode
    , decode'
    , chop
    , unchop
    ) where

import Data.Word

import Codec.Binary.Util

-- {{{1 encode
-- | Encode data.  Each nibble of the byte is converted into one character in
-- [0-9A-F].
encode :: [Word8]
    -> String
encode [] = ""
encode (o:os) = toHex o ++ encode os

-- {{{1 decode
-- | Decode data (lazy).
decode' :: String
    -> [Maybe Word8]
decode' "" = []
decode' (hn:ln:cs) = fromHex [hn, ln] : decode' cs
decode' _ = [Nothing]

-- | Decode data (strict).
decode :: String -> Maybe [Word8]
decode = sequence . decode'

-- {{{1 chop
-- | Chop up a string in parts.
chop :: Int     -- ^ length of individual lines
    -> String
    -> [String]
chop n "" = []
chop n s = let
        enc_len | n < 2 = 2
                | otherwise = n `div` 2 * 2
    in (take enc_len s) : chop n (drop enc_len s)

-- {{{1 unchop
-- | Concatenate the list of strings into one long string.
unchop :: [String] -> String
unchop = foldr (++) ""
