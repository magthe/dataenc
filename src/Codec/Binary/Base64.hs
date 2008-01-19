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

-- | Base64 module.
--
--   Implemented as specified in RFC 4648
--   (<http://tools.ietf.org/html/rfc4648>).
module Codec.Binary.Base64
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

-- {{{1 encodeArray
encodeArray :: Array Word8 Char
encodeArray = array (0, 64) [
    (0, 'A'), (1, 'B'), (2, 'C'), (3, 'D'), (4, 'E'),
    (5, 'F') , (6, 'G'), (7, 'H'), (8, 'I'), (9, 'J'),
    (10, 'K'), (11, 'L'), (12, 'M'), (13, 'N'), (14, 'O'),
    (15, 'P'), (16, 'Q'), (17, 'R'), (18, 'S'), (19, 'T'),
    (20, 'U'), (21, 'V'), (22, 'W'), (23, 'X'), (24, 'Y'),
    (25, 'Z'), (26, 'a'), (27, 'b'), (28, 'c'), (29, 'd'),
    (30, 'e'), (31, 'f'), (32, 'g'), (33, 'h'), (34, 'i'),
    (35, 'j'), (36, 'k'), (37, 'l'), (38, 'm'), (39, 'n'),
    (40, 'o'), (41, 'p'), (42, 'q'), (43, 'r'), (44, 's'),
    (45, 't'), (46, 'u'), (47, 'v'), (48, 'w'), (49, 'x'),
    (50, 'y'), (51, 'z'), (52, '0'), (53, '1'), (54, '2'),
    (55, '3'), (56, '4'), (57, '5'), (58, '6'), (59, '7'),
    (60, '8'), (61, '9'), (62, '+'), (63, '/') ]

-- {{{1 decodeMap
decodeMap :: M.Map Char Word8
decodeMap  = M.fromList [
    ('A', 0), ('B', 1), ('C', 2), ('D', 3), ('E', 4),
    ('F', 5), ('G', 6), ('H', 7), ('I', 8), ('J', 9),
    ('K', 10), ('L', 11), ('M', 12), ('N', 13), ('O', 14),
    ('P', 15), ('Q', 16), ('R', 17), ('S', 18), ('T', 19),
    ('U', 20), ('V', 21), ('W', 22), ('X', 23), ('Y', 24),
    ('Z', 25), ('a', 26), ('b', 27), ('c', 28), ('d', 29),
    ('e', 30), ('f', 31), ('g', 32), ('h', 33), ('i', 34),
    ('j', 35), ('k', 36), ('l', 37), ('m', 38), ('n', 39),
    ('o', 40), ('p', 41), ('q', 42), ('r', 43), ('s', 44),
    ('t', 45), ('u', 46), ('v', 47), ('w', 48), ('x', 49),
    ('y', 50), ('z', 51), ('0', 52), ('1', 53), ('2', 54),
    ('3', 55), ('4', 56), ('5', 57), ('6', 58), ('7', 59),
    ('8', 60), ('9', 61), ('+', 62), ('/', 63) ]

-- {{{1 encode
-- | Encode data.
encode :: [Word8]
    -> String
encode [] = ""
encode [o] = let
        i1 = o `shiftR` 2
        i2 = o `shiftL` 4 .&. 0x3f
    in (encodeArray ! i1) : (encodeArray ! i2) : "=="
encode [o1, o2] = let
        i1 = o1 `shiftR` 2
        i2 = (o1 `shiftL` 4 .|. o2 `shiftR` 4) .&. 0x3f
        i3 = o2 `shiftL` 2 .&. 0x3f
    in foldr (\ i s-> (encodeArray ! i) : s) "=" [i1, i2, i3]
encode (o1:o2:o3:os) = let
        i1 = o1 `shiftR` 2
        i2 = (o1 `shiftL` 4 .|. o2 `shiftR` 4) .&. 0x3f
        i3 = (o2 `shiftL` 2 .|. o3 `shiftR` 6) .&. 0x3f
        i4 = o3 .&. 0x3f
    in (foldr (\ i s -> (encodeArray ! i) : s) "" [i1, i2, i3, i4]) ++ encode os

-- {{{1 decode
-- | Decode data (lazy).
decode' :: String
    -> [Maybe Word8]
decode' = let
        dec [] = []
        dec [Just eo1, Just eo2] = [Just (eo1 `shiftL` 2 .|. eo2 `shiftR` 4)]
        dec [Just eo1, Just eo2, Just eo3] = let
                o1 = eo1 `shiftL` 2 .|. eo2 `shiftR` 4
                o2 = eo2 `shiftL` 4 .|. eo3 `shiftR` 2
            in [Just o1, Just o2]
        dec (Just eo1:Just eo2:Just eo3:Just eo4:eos) = let
                o1 = eo1 `shiftL` 2 .|. eo2 `shiftR` 4
                o2 = eo2 `shiftL` 4 .|. eo3 `shiftR` 2
                o3 = eo3 `shiftL` 6 .|. eo4
            in Just o1:Just o2:Just o3:(dec eos)
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
--   The length given is rounded down to the nearest multiple of 4.
--
--   /Notes:/
--
--   * PEM requires lines that are 64 characters long.
--
--   * MIME requires lines that are at most 76 characters long.
chop :: Int     -- ^ length of individual lines
    -> String
    -> [String]
chop n "" = []
chop n s = let
        enc_len | n < 4 = 4
                | otherwise = n `div` 4 * 4
    in (take enc_len s) : chop n (drop enc_len s)

-- {{{1 unchop
-- | Concatenate the strings into one long string.
unchop :: [String]
    -> String
unchop = foldr (++) ""
