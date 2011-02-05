-- |
-- Module    : Codec.Binary.Url
-- Copyright : (c) 2009 Magnus Therning
-- License   : BSD3
--
-- URL encoding, sometimes referred to as URI encoding or percent encoding.
-- Implemented based on RFC 3986 (<http://tools.ietf.org/html/rfc3986>).
--
-- Further documentation and information can be found at
-- <http://www.haskell.org/haskellwiki/Library/Data_encoding>.

module Codec.Binary.Url
    ( encode
    , DecIncData
    , DecIncRes
    , decodeInc
    , decode
    , chop
    , unchop
    ) where

import Codec.Binary.Util

import qualified Data.Map as M
import Data.Char(ord)
import Data.Word(Word8)
import Data.Maybe(isJust, fromJust)

_unreservedChars = zip [65..90] "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        ++ zip [97..122] "abcdefghijklmnopqrstuvwxyz"
        ++ zip [48..57] "0123456789"
        ++ [(45, '-'), (95, '_'), (46, '.'), (126, '~')]

encodeMap :: M.Map Word8 Char
encodeMap = M.fromList _unreservedChars

decodeMap :: M.Map Char Word8
decodeMap = M.fromList [(b, a) | (a, b) <- _unreservedChars]

-- {{{1 encode
-- | Encode data.
encode :: [Word8]
    -> String
encode [] = ""
encode (o : os) = case (M.lookup o encodeMap) of
    Just c -> c : encode os
    Nothing -> ('%' : toHex o) ++ encode os

-- {{{1 decode
decodeInc :: DecIncData String -> DecIncRes String
decodeInc d = dI [] d
    where
        dI [] Done = Final [] []
        dI lo Done = Fail [] lo
        dI lo (Chunk s) = doDec [] (lo ++ s)
            where
                doDec acc [] = Part acc (dI [])
                doDec acc s'@('%':c0:c1:cs) = let
                        o = fromHex [c0, c1]
                    in if isJust o
                        then doDec (acc ++ [fromJust o]) cs
                        else Fail acc s'
                doDec acc s'@(c:cs)
                    | c /= '%' = doDec (acc ++ [fromIntegral $ ord c]) cs
                    | otherwise = Part acc (dI s')

-- | Decode data (strict).
decode :: String
    -> Maybe [Word8]
decode = decoder decodeInc

-- {{{1 chop
-- | Chop up a string in parts.
chop :: Int     -- ^ length of individual lines
    -> String
    -> [String]
chop n = let
        _n = max 1 n
        _chop [] = []
        _chop cs = take _n cs : _chop (drop _n cs)
    in _chop

-- {{{1 unchop
-- | Concatenate the strings into one long string
unchop :: [String]
    -> String
unchop = foldr (++) ""
