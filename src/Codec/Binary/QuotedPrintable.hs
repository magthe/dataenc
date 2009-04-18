-- |
-- Module    : Codec.Binary.QuotedPrintable
-- Copyright : (c) 2009 Magnus Therning
-- License   : BSD3
--
-- Implementation of Quoted-Printable based on RFC 2045
-- (<http://tools.ietf.org/html/rfc2045>).
--
-- This encoding encodes _everything_ that is passed in, it will not try to
-- guess the native line ending for your architecture.  In other words, if you
-- are using this to encode text you need to split it into separate lines
-- before encoding and chopping it up.
--
-- Further documentation and information can be found at
-- <http://www.haskell.org/haskellwiki/Library/Data_encoding>.
module Codec.Binary.QuotedPrintable
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
    | o < 33 || o == 61 || o > 126 = ('=' : toHex o) ++ encode os
    | otherwise = chr (fromIntegral o) : encode os


-- {{{1 decode
-- -- | Decode data (lazy).
decode' :: String
    -> [Maybe Word8]
decode' [] = []
decode' ('=':c0:c1:cs) = fromHex [c0, c1] : decode' cs
decode' (c:cs)
    | c /= '=' = (Just $ fromIntegral $ ord c) : decode' cs
    | otherwise = [Nothing]

-- | Decode data (strict).
decode :: String -> Maybe [Word8]
decode = sequence . decode'

-- {{{1 chop
-- | Chop up a string in parts.
chop :: Int     -- ^ length of individual lines (values below 4 are ignored)
    -> String
    -> [String]
chop n "" = []
chop n s = let
        n' = max 3 $ n - 1
        _c i ts "" acc = ts : acc
        _c i ts tss@('=':tss') acc
            | i + 2 < n' = _c (i + 1) ('=' : ts) tss' acc
            | otherwise = _c 0 "" tss (('=' : ts) : acc)
        _c i ts tss@(c:tss') acc
            | i < n' = _c (i + 1) (c : ts) tss' acc
            | otherwise = _c 0 "" tss (('=' : ts) : acc)
    in map reverse . reverse $ _c 0 "" s []

-- {{{1 unchop
-- | Concatenate the list of strings into one long string.
unchop :: [String] -> String
unchop [] = ""
unchop (s:ss) = let
        dropLast = last s == '='
        len = length s
    in if dropLast
        then (take (len - 1) s) ++ (unchop ss)
        else s ++ (unchop ss)
