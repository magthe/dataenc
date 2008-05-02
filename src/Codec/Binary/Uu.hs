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

-- | Uuencoding module.
--
--   Uuencoding is notoriously badly specified.  This implementation is
--   compatible with the GNU Sharutils
--   (<http://www.gnu.org/software/sharutils/>).
module Codec.Binary.Uu
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
    [ (0, '`'), (1, '!'), (2, '"'), (3, '#'), (4, '$')
    , (5, '%'), (6, '&'), (7, '\''), (8, '('), (9, ')')
    , (10, '*'), (11, '+'), (12, ','), (13, '-'), (14, '.')
    , (15, '/'), (16, '0'), (17, '1'), (18, '2'), (19, '3')
    , (20, '4'), (21, '5'), (22, '6'), (23, '7'), (24, '8')
    , (25, '9'), (26, ':'), (27, ';'), (28, '<'), (29, '=')
    , (30, '>'), (31, '?'), (32, '@'), (33, 'A'), (34, 'B')
    , (35, 'C'), (36, 'D'), (37, 'E'), (38, 'F'), (39, 'G')
    , (40, 'H'), (41, 'I'), (42, 'J'), (43, 'K'), (44, 'L')
    , (45, 'M'), (46, 'N'), (47, 'O'), (48, 'P'), (49, 'Q')
    , (50, 'R'), (51, 'S'), (52, 'T'), (53, 'U') ,(54, 'V')
    , (55, 'W'), (56, 'X'), (57, 'Y'), (58, 'Z'), (59, '[')
    , (60, '\\'), (61, ']'), (62, '^'), (63, '_') ]

-- {{{1 encodeArray
encodeArray :: Array Word8 Char
encodeArray = array (0, 64) _encMap

-- {{{1 decodeMap
decodeMap :: M.Map Char Word8
decodeMap = M.fromList [(snd i, fst i) | i <- _encMap]

-- {{{1 encode
-- | Encode data.
encode :: [Word8]
    -> String
encode [] = ""
encode [o] = let
        i1 = o `shiftR` 2
        i2 = o `shiftL` 4 .&. 0x3f
    in (encodeArray ! i1) : (encodeArray ! i2) : ""
encode [o1, o2] = let
        i1 = o1 `shiftR` 2
        i2 = (o1 `shiftL` 4 .|. o2 `shiftR` 4) .&. 0x3f
        i3 = o2 `shiftL` 2 .&. 0x3f
    in foldr (\ i s-> (encodeArray ! i) : s) "" [i1, i2, i3]
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
        dec . map (flip M.lookup decodeMap)

-- | Decode data (strict).
decode :: String
    -> Maybe [Word8]
decode = sequence . decode'

-- {{{1 chop
-- | Chop up a string in parts.  Each string in the resulting list is prepended
--   with the length according to the uuencode \"specificiation\".
--
--   /Notes:/
--
--   * The length of the strings in the result will be @(n -1) `div` 4 * 4@.
--   The @-1@ comes from the need to prepend the length.  Keeping it to a
--   multiple of 4 means that strings returned from 'encode' can be chopped
--   without requiring any changes.
--
--   * The length of lines in GNU's sharutils is 61.
chop :: Int     -- ^ length (@1 < n <= 65@, not checked)
    -> String
    -> [String]
chop n "" = []
chop n s = let
        enc_len | n < 5 = 4
                | otherwise = (n - 1) `div` 4 * 4
        enc_line = take enc_len s
        act_len = fromIntegral $ case (length enc_line `divMod` 4) of
            (l, 0) -> l * 3
            (l, 2) -> l * 3 + 1
            (l, 3) -> l * 3 + 2
        len = (encodeArray ! act_len)
    in (len : enc_line) : chop n (drop enc_len s)

-- {{{1 unchop
-- | Concatenate the strings into one long string.  Each string is assumed to
--   be prepended with the length according to the uuencode specification.
unchop :: [String]
    -> String
unchop ss = let
        singleUnchop (l:cs) = let
                act_len = fromIntegral $ decodeMap M.! l
                enc_len = case (act_len `divMod` 3) of
                    (n, 0) -> n * 4
                    (n, 1) -> n * 4 + 2
                    (n, 2) -> n * 4 + 3
            in take enc_len cs
    in foldr (\ s res -> (singleUnchop s) ++ res) "" ss
