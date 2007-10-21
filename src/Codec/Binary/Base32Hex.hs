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

-- | Base32Hex module.
--
--   Implemented as specified in RFC 4648
--   (<http://tools.ietf.org/html/rfc4648>).
module Codec.Binary.Base32Hex
    ( encode
    , decode
    , chop
    , unchop
    ) where

import Control.Monad
import Data.Array
import Data.Bits
import Data.Maybe
import Data.Word
import qualified Data.Map as M

import qualified Codec.Binary.Base32 as Base32

-- {{{1 encodeArray
encodeArray :: Array Word8 Char
encodeArray = array (0, 32) [
    (0, '0'), (1, '1'), (2, '2'), (3, '3'), (4, '4'),
    (5, '5'), (6, '6'), (7, '7'), (8, '8'), (9, '9'),
    (10, 'A'), (11, 'B'), (12, 'C'), (13, 'D'), (14, 'E'),
    (15, 'F'), (16, 'G'), (17, 'H'), (18, 'I'), (19, 'J'),
    (20, 'K'), (21, 'L'), (22, 'M'), (23, 'N'), (24, 'O'),
    (25, 'P'), (26, 'Q'), (27, 'R'), (28, 'S'), (29, 'T'),
    (30, 'U'), (31, 'V') ]

-- {{{1 decodeMap
decodeMap :: M.Map Char Word8
decodeMap = M.fromList [
    ('0', 0), ('1', 1), ('2', 2), ('3', 3), ('4', 4),
    ('5', 5), ('6', 6), ('7', 7), ('8', 8), ('9', 9),
    ('A', 10), ('B', 11), ('C', 12), ('D', 13), ('E', 14),
    ('F', 15), ('G', 16), ('H', 17), ('I', 18), ('J', 19),
    ('K', 20), ('L', 21), ('M', 22), ('N', 23), ('O', 24),
    ('P', 25), ('Q', 26), ('R', 27), ('S', 28), ('T', 29),
    ('U', 30), ('V', 31)
    ]

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
-- | Decode data.
decode :: String
    -> Maybe [Word8]
decode s = let
        dec [] = []
        dec [eo1, eo2] = [eo1 `shiftL` 3 .|. eo2 `shiftR` 2]
        dec [eo1, eo2, eo3, eo4] = let
                o1 = eo1 `shiftL` 3 .|. eo2 `shiftR` 2
                o2 = eo2 `shiftL` 6 .|. eo3 `shiftL` 1 .|. eo4 `shiftR` 4
            in [o1, o2]
        dec [eo1, eo2, eo3, eo4, eo5] = let
                o1 = eo1 `shiftL` 3 .|. eo2 `shiftR` 2
                o2 = eo2 `shiftL` 6 .|. eo3 `shiftL` 1 .|. eo4 `shiftR` 4
                o3 = eo4 `shiftL` 4 .|. eo5 `shiftR` 1
            in [o1, o2, o3]
        dec [eo1, eo2, eo3, eo4, eo5, eo6, eo7] = let
                o1 = eo1 `shiftL` 3 .|. eo2 `shiftR` 2
                o2 = eo2 `shiftL` 6 .|. eo3 `shiftL` 1 .|. eo4 `shiftR` 4
                o3 = eo4 `shiftL` 4 .|. eo5 `shiftR` 1
                o4 = eo5 `shiftL` 7 .|. eo6 `shiftL` 2 .|. eo7 `shiftR` 3
            in [o1, o2, o3, o4]
        dec (eo1:eo2:eo3:eo4:eo5:eo6:eo7:eo8:eos) = let
                o1 = eo1 `shiftL` 3 .|. eo2 `shiftR` 2
                o2 = eo2 `shiftL` 6 .|. eo3 `shiftL` 1 .|. eo4 `shiftR` 4
                o3 = eo4 `shiftL` 4 .|. eo5 `shiftR` 1
                o4 = eo5 `shiftL` 7 .|. eo6 `shiftL` 2 .|. eo7 `shiftR` 3
                o5 = eo7 `shiftL` 5 .|. eo8
            in o1:o2:o3:o4:o5:(dec eos)
    in
        if (length s `mod` 8 == 0)
            then liftM dec $ sequence $ map (flip M.lookup decodeMap) (filter (/= '=') s)
            else Nothing

-- {{{1 chop
-- | Chop up a string in parts.
--
--   See 'Base32.chop' in "Base32" for more details.
chop :: Int     -- ^ length of individual lines
    -> String
    -> [String]
chop = Base32.chop

-- {{{1 unchop
-- | Concatenate the strings into one long string.
--
--   See 'Base32.unchop' in "Codec.Binary.Base32" for more details.
unchop :: [String]
    -> String
unchop = Base32.unchop
