{- Copyright © 2008 Magnus Therning
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

-- | Base85 module.
--
--   Implemented as described at http://en.wikipedia.org/wiki/Ascii85.
module Codec.Binary.Base85
    ( encode
    , decode
    , decode'
    , chop
    , unchop
    ) where

import Data.Array
import Data.Bits
import Data.Char
import Data.Maybe
import Data.Word
import qualified Data.Map as M

-- {{{1 enc/dec map
_encMap :: [(Word8, Char)]
_encMap = [(fromIntegral i, chr i) | i <- [33..117]]

-- {{{1 encodeArray
encodeArray :: Array Word8 Char
encodeArray = array (33, 117) _encMap

-- {{{1 decodeMap
decodeMap :: M.Map Char Word8
decodeMap = M.fromList [(snd i, fst i) | i <- _encMap]

-- {{{1 encode
-- | Encode data.
encode :: [Word8]
    -> String
encode [] = ""
encode [b1] = take 2 $ encode [b1, 0, 0, 1]
encode [b1, b2] = take 3 $ encode [b1, b2, 0, 1]
encode [b1, b2, b3] = take 4 $ encode [b1, b2, b3, 1]
encode (0:0:0:0:bs) = "z" ++ encode bs
encode (b1:b2:b3:b4:bs) = (foldr (\ i s -> (encodeArray ! i) : s) "" group) ++ (encode bs)
    where
        --(reverse . map (\ i -> i + 33) . map (`mod` 85) . take 5 . iterate (`div` 85) . foldl1 (\ a b -> a `shiftL` 8 + b)) [b1, b2, b3, b4]
        group2Int :: Int
        group2Int = foldl (\ a b -> a `shiftL` 8 + (fromIntegral b)) 0 [b1, b2, b3, b4]
        encodeInt2Ints :: Int -> [Word8]
        encodeInt2Ints = map fromIntegral . map (`mod` 85) . take 5 . iterate (`div` 85)
        adjustNReverse = reverse . map (+ 33)
        group = (adjustNReverse .encodeInt2Ints) group2Int

-- {{{1 decode
-- | Decode data (lazy).
decode' :: String
    -> [Maybe Word8]
decode' [] = []
decode' ('z':cs) = (Just 0:Just 0:Just 0:Just 0:decode' cs)
decode' cs = let
        pad n = take n $ repeat $ Just 0
        dec :: [Maybe Word8] -> [Maybe Word8]
        --dec [] = []
        dec l@[Just c1, Just c2] = take 1 . dec $ l ++ pad 3
        dec l@[Just c1, Just c2, Just c3] = take 2 . dec $ l ++ pad 2
        dec l@[Just c1, Just c2, Just c3, Just c4] = take 3 . dec $ l ++ pad 1
        dec l@[Just c1, Just c2, Just c3, Just c4, Just c5] = let
                adjRev = map (flip (-) 33) [c5, c4, c3, c2, c1]
                group2Int :: [Word8] -> Int
                group2Int = foldl1 (+) . map (uncurry (*)) . zip (map (85 ^) [0..4]) . map fromIntegral
                int2Group :: Int -> [Maybe Word8]
                int2Group = map Just . map fromIntegral . reverse . map (`mod` 256) . take 4 . iterate (`div` 256)
            in (int2Group . group2Int) adjRev
        dec _ = [Nothing]
    in (dec . map (flip M.lookup decodeMap) $ take 5 cs) ++ decode' (drop 5 cs)

-- | Decode data (strict).
decode :: String
    -> Maybe [Word8]
decode = sequence . decode'

-- {{{1 chop
-- | Chop up a string in parts.
--
--   The length given is rounded down to the nearest multiple of 5.
chop :: Int     -- ^ length of individual lines
    -> String
    -> [String]
chop _ "" = []
chop n s = let
        enc_len | n < 5 = 5
                | otherwise = n `div` 5 * 5
    in (take enc_len s) : chop n (drop enc_len s)

-- {{{1 unchop
-- | Concatenate the strings into one long string.
unchop :: [String]
    -> String
unchop = foldr (++) ""
