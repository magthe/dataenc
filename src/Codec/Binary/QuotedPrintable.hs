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
-- | Incremental encoder function.
encodeInc :: EncIncData -> EncIncRes String
encodeInc e = eI e
    where
        enc [] = []
        enc (o:os)
            | o < 33 || o == 61 || o > 126 = ('=' : toHex o) ++ enc os
            | otherwise = chr (fromIntegral o) : enc os

        eI EDone = EFinal []
        eI (EChunk bs) = EPart (enc bs) encodeInc

-- | Encode data.
encode :: [Word8] -> String
encode = encoder encodeInc

-- {{{1 decode
-- | Incremental decoder function.
decodeInc :: DecIncData String -> DecIncRes String
decodeInc d = dI [] d
    where
        dI [] DDone = DFinal [] []
        dI lo DDone = DFail [] lo
        dI lo (DChunk s) = doDec [] (lo ++ s)
            where
                doDec acc [] = DPart (reverse $ concat acc) (dI [])
                doDec acc s'@('=':c0:c1:cs) = let
                        o = fromHex [c0, c1]
                    in if isJust o
                        then doDec ([fromJust o] : acc) cs
                        else DFail (reverse $ concat acc) s'
                doDec acc s'@(c:cs)
                    | c /= '=' = doDec ([fromIntegral $ ord c] : acc) cs
                    | otherwise = DPart (reverse $ concat acc) (dI s')

-- | Decode data.
decode :: String -> Maybe [Word8]
decode = decoder decodeInc

-- {{{1 chop
-- | Chop up a string in parts.
chop :: Int     -- ^ length of individual lines (values @\< 4@ are ignored)
    -> String
    -> [String]
chop n "" = []
chop n s = let
        n' = max 3 $ n - 1
        _c i ts "" acc = ts : acc
        _c i ts tss@('=' : tss') acc
            | i + 2 < n' = _c (i + 1) ('=' : ts) tss' acc
            | otherwise = _c 0 "" tss (('=' : ts) : acc)
        _c i ts tss@(c : tss') acc
            | i < n' = _c (i + 1) (c : ts) tss' acc
            | otherwise = _c 0 "" tss (('=' : ts) : acc)
    in map reverse . reverse $ _c 0 "" s []

-- {{{1 unchop
-- | Concatenate the list of strings into one long string.
unchop :: [String] -> String
unchop [] = ""
unchop (s : ss) = let
        dropLast = last s == '='
        len = length s
    in if dropLast
        then take (len - 1) s ++ unchop ss
        else s ++ unchop ss
