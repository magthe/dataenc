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

-- | Base16 module.
--
--   Implemented as specified in RFC 4648
--   (<http://tools.ietf.org/html/rfc4648>).
module Codec.Binary.Base16
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
    [ (0, '0'), (1, '1'), (2, '2'), (3, '3'), (4, '4')
    , (5, '5'), (6, '6'), (7, '7'), (8, '8'), (9, '9')
    , (10, 'A'), (11, 'B'), (12, 'C'), (13, 'D'), (14, 'E')
    , (15, 'F') ]

-- {{{1 encodeArray
encodeArray :: Array Word8 Char
encodeArray = array (0, 64) _encMap

-- {{{1 decodeMap
decodeMap :: M.Map Char Word8
decodeMap  = M.fromList [(snd i, fst i) | i <- _encMap]

-- {{{1 encode
-- | Encode data.
encode :: [Word8]
    -> String
encode os = let
        splitOctet o = [ o `shiftR` 4, o .&. 0xf ]
    in map (encodeArray !) $ foldr (\ o l -> (splitOctet o) ++ l) [] os

-- {{{1 decode
-- | Decode data (lazy).
decode' :: String
    -> [Maybe Word8]
decode' = let
        dec [] = []
        dec (Just o1:Just o2:os) =
                    Just (o1 `shiftL` 4 .|. o2) : dec os
        dec _ = [Nothing]
    in
        dec . map (flip M.lookup decodeMap)

-- | Decode data (strict).
decode :: String
    -> Maybe [Word8]
decode = sequence . decode'

-- {{{1 chop
-- | Chop up a string in parts.
--
--   The length given is rounded down to the nearest multiple of 2.
chop :: Int     -- ^ length of individual lines
    -> String
    -> [String]
chop n "" = []
chop n s = let
        enc_len | n < 2 = 2
                | otherwise = n `div` 2 * 2
    in (take enc_len s) : chop n (drop enc_len s)

-- {{{1 unchop
-- | Concatenate the strings into one long string.
unchop :: [String]
    -> String
unchop = foldr (++) ""
