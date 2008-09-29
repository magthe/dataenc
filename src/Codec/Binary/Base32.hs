-- |
-- Module    : Codec.Binary.Base32
-- Copyright : (c) 2007 Magnus Therning
-- License   : BSD3
--
-- Implemented as specified in RFC 4648
-- (<http://tools.ietf.org/html/rfc4648>).
--
-- Further documentation and information can be found at
-- <http://www.haskell.org/haskellwiki/Library/Data_encoding>.
module Codec.Binary.Base32
    ( encode
    , decode
    , decode'
    , chop
    , unchop
    ) where

import Control.Monad
import Data.Array
import Data.Bits
import Data.Maybe
import Data.Word
import qualified Data.Map as M

-- {{{1 enc/dec map
_encMap =
    [ (0, 'A'), (1, 'B'), (2, 'C'), (3, 'D'), (4, 'E')
    , (5, 'F'), (6, 'G'), (7, 'H'), (8, 'I'), (9, 'J')
    , (10, 'K'), (11, 'L'), (12, 'M'), (13, 'N'), (14, 'O')
    , (15, 'P'), (16, 'Q'), (17, 'R'), (18, 'S'), (19, 'T')
    , (20, 'U'), (21, 'V'), (22, 'W'), (23, 'X'), (24, 'Y')
    , (25, 'Z'), (26, '2'), (27, '3'), (28, '4'), (29, '5')
    , (30, '6'), (31, '7') ]

-- {{{1 encodeArray
encodeArray :: Array Word8 Char
encodeArray = array (0, 32) _encMap

-- {{{1 decodeMap
decodeMap :: M.Map Char Word8
decodeMap = M.fromList [(snd i, fst i) | i <- _encMap]

-- {{{1 encode
-- | Encode data.
encode :: [Word8]
    -> String
encode = let
        pad n = take n $ repeat 0
        enc [] = ""
        enc l@[o] = (++ "======") . take 2 . enc $ l ++ pad 4
        enc l@[o1, o2] = (++ "====") . take 4 . enc $ l ++ pad 3
        enc l@[o1, o2, o3] = (++ "===") . take 5 . enc $ l ++ pad 2
        enc l@[o1, o2, o3, o4] = (++ "=") . take 7 . enc $ l ++ pad 1
        enc (o1:o2:o3:o4:o5:os) = let
                i1 = o1 `shiftR` 3
                i2 = (o1 `shiftL` 2 .|. o2 `shiftR` 6) .&. 0x1f
                i3 = o2 `shiftR` 1 .&. 0x1f
                i4 = (o2 `shiftL` 4 .|. o3 `shiftR` 4) .&. 0x1f
                i5 = (o3 `shiftL` 1 .|. o4 `shiftR` 7) .&. 0x1f
                i6 = o4 `shiftR` 2 .&. 0x1f
                i7 = (o4 `shiftL` 3 .|. o5 `shiftR` 5) .&. 0x1f
                i8 = o5 .&. 0x1f
            in (foldr (\ i s -> (encodeArray ! i) : s) "" [i1, i2, i3, i4, i5, i6, i7, i8]) ++ (enc os)
    in enc

-- {{{1 decode
-- | Decode data (lazy).
decode' :: String
    -> [Maybe Word8]
decode' = let
        pad n = take n $ repeat $ Just 0
        dec [] = []
        dec l@[Just eo1, Just eo2] = take 1 . dec $ l ++ pad 6
        dec l@[Just eo1, Just eo2, Just eo3, Just eo4] = take 2 . dec $ l ++ pad 4
        dec l@[Just eo1, Just eo2, Just eo3, Just eo4, Just eo5] = take 3 . dec $ l ++ pad 3
        dec l@[Just eo1, Just eo2, Just eo3, Just eo4, Just eo5, Just eo6, Just eo7] = take 4 . dec $ l ++ pad 1
        dec (Just eo1:Just eo2:Just eo3:Just eo4:Just eo5:Just eo6:Just eo7:Just eo8:eos) = let
                o1 = eo1 `shiftL` 3 .|. eo2 `shiftR` 2
                o2 = eo2 `shiftL` 6 .|. eo3 `shiftL` 1 .|. eo4 `shiftR` 4
                o3 = eo4 `shiftL` 4 .|. eo5 `shiftR` 1
                o4 = eo5 `shiftL` 7 .|. eo6 `shiftL` 2 .|. eo7 `shiftR` 3
                o5 = eo7 `shiftL` 5 .|. eo8
            in Just o1:Just o2:Just o3:Just o4:Just o5:(dec eos)
        dec _ = [Nothing]
    in
        dec . map (flip M.lookup decodeMap) . takeWhile (/= '=')

-- | Decode data (strict).
decode :: String
    -> Maybe [Word8]
decode = sequence . decode'

-- {{{1 chop
-- | Chop up a string in parts.
--
--   The length given is rounded down to the nearest multiple of 8.
chop :: Int     -- ^ length of individual lines
    -> String
    -> [String]
chop n "" = []
chop n s = let
        enc_len | n < 8 = 8
                | otherwise = n `div` 8 * 8
    in (take enc_len s) : chop n (drop enc_len s)

-- {{{1 unchop
-- | Concatenate the strings into one long string.
unchop :: [String]
    -> String
unchop = foldr (++) ""
