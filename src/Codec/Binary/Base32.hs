{- Copyright © 2007 Magnus Therning
 -
 - This file is part of dataenc.
 -
 - Dataenc is free software: you can redistribute it and/or modify it under
 - the terms of the GNU Lesser General Public License as published by the
 - Free Software Foundation, either version 3 of the License, or (at your
 - option) any later version.
 -
 - Dataenc is distributed in the hope that it will be useful, but WITHOUT
 - ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 - FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 - License for more details.
 -
 - You should have received a copy of the GNU Lesser General Public License
 - along with dataenc.  If not, see <http://www.gnu.org/licenses/>
 -}

-- | Base32 module.
--
--   Implemented as specified in RFC 4648
--   (<http://tools.ietf.org/html/rfc4648>).
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
encode [] = ""
encode [o] = let
        i1 = o `shiftR` 3
        i2 = o `shiftL` 2 .&. 0x1f
    in (encodeArray ! i1) : (encodeArray ! i2) : "======"
encode [o1, o2] = let
        i1 = o1 `shiftR` 3
        i2 = (o1 `shiftL` 2 .|. o2 `shiftR` 6) .&. 0x1f
        i3 = o2 `shiftR` 1 .&. 0x1f
        i4 = o2 `shiftL` 4 .&. 0x1f
    in foldr (\ i s -> (encodeArray ! i) : s) "====" [i1, i2, i3, i4]
encode [o1, o2, o3] = let
        i1 = o1 `shiftR` 3
        i2 = (o1 `shiftL` 2 .|. o2 `shiftR` 6) .&. 0x1f
        i3 = o2 `shiftR` 1 .&. 0x1f
        i4 = (o2 `shiftL` 4 .|. o3 `shiftR` 4) .&. 0x1f
        i5 = o3 `shiftL` 1 .&. 0x1f
    in foldr (\ i s -> (encodeArray ! i) : s) "===" [i1, i2, i3, i4, i5]
encode [o1, o2, o3, o4] = let
        i1 = o1 `shiftR` 3
        i2 = (o1 `shiftL` 2 .|. o2 `shiftR` 6) .&. 0x1f
        i3 = o2 `shiftR` 1 .&. 0x1f
        i4 = (o2 `shiftL` 4 .|. o3 `shiftR` 4) .&. 0x1f
        i5 = (o3 `shiftL` 1 .|. o4 `shiftR` 7) .&. 0x1f
        i6 = o4 `shiftR` 2 .&. 0x1f
        i7 = o4 `shiftL` 3 .&. 0x1f
    in foldr (\ i s -> (encodeArray ! i) : s) "=" [i1, i2, i3, i4, i5, i6, i7]
encode (o1:o2:o3:o4:o5:os) = let
        i1 = o1 `shiftR` 3
        i2 = (o1 `shiftL` 2 .|. o2 `shiftR` 6) .&. 0x1f
        i3 = o2 `shiftR` 1 .&. 0x1f
        i4 = (o2 `shiftL` 4 .|. o3 `shiftR` 4) .&. 0x1f
        i5 = (o3 `shiftL` 1 .|. o4 `shiftR` 7) .&. 0x1f
        i6 = o4 `shiftR` 2 .&. 0x1f
        i7 = (o4 `shiftL` 3 .|. o5 `shiftR` 5) .&. 0x1f
        i8 = o5 .&. 0x1f
    in (foldr (\ i s -> (encodeArray ! i) : s) "" [i1, i2, i3, i4, i5, i6, i7, i8]) ++ (encode os)

-- {{{1 decode
-- | Decode data (lazy).
decode' :: String
    -> [Maybe Word8]
decode' = let
        dec [] = []
        dec [Just eo1, Just eo2] = [Just (eo1 `shiftL` 3 .|. eo2 `shiftR` 2)]
        dec [Just eo1, Just eo2, Just eo3, Just eo4] = let
                o1 = eo1 `shiftL` 3 .|. eo2 `shiftR` 2
                o2 = eo2 `shiftL` 6 .|. eo3 `shiftL` 1 .|. eo4 `shiftR` 4
            in [Just o1, Just o2]
        dec [Just eo1, Just eo2, Just eo3, Just eo4, Just eo5] = let
                o1 = eo1 `shiftL` 3 .|. eo2 `shiftR` 2
                o2 = eo2 `shiftL` 6 .|. eo3 `shiftL` 1 .|. eo4 `shiftR` 4
                o3 = eo4 `shiftL` 4 .|. eo5 `shiftR` 1
            in [Just o1, Just o2, Just o3]
        dec [Just eo1, Just eo2, Just eo3, Just eo4, Just eo5, Just eo6, Just eo7] = let
                o1 = eo1 `shiftL` 3 .|. eo2 `shiftR` 2
                o2 = eo2 `shiftL` 6 .|. eo3 `shiftL` 1 .|. eo4 `shiftR` 4
                o3 = eo4 `shiftL` 4 .|. eo5 `shiftR` 1
                o4 = eo5 `shiftL` 7 .|. eo6 `shiftL` 2 .|. eo7 `shiftR` 3
            in [Just o1, Just o2, Just o3, Just o4]
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
