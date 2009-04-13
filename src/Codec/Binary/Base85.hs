-- |
-- Module    : Codec.Binary.Base85
-- Copyright : (c) 2007 Magnus Therning
-- License   : BSD3
--
-- Implemented as described at <http://en.wikipedia.org/wiki/Ascii85>.
--
-- Further documentation and information can be found at
-- <http://www.haskell.org/haskellwiki/Library/Data_encoding>.
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
--
--   The result will not be enclosed in \<~ ~\>.
encode :: [Word8]
    -> String
encode [] = ""
encode [b1] = take 2 $ encode [b1, 0, 0, 1]
encode [b1, b2] = take 3 $ encode [b1, b2, 0, 1]
encode [b1, b2, b3] = take 4 $ encode [b1, b2, b3, 1]
encode (0:0:0:0:bs) = "z" ++ encode bs
encode (b1:b2:b3:b4:bs) = (foldr (\ i s -> (encodeArray ! i) : s) "" group) ++ (encode bs)
    where
        group2Word32 :: Word32
        group2Word32 = foldl (\ a b -> a `shiftL` 8 + (fromIntegral b)) 0 [b1, b2, b3, b4]
        encodeWord32ToWord8s :: Word32 -> [Word8]
        encodeWord32ToWord8s = map fromIntegral . map (`mod` 85) . take 5 . iterate (`div` 85)
        adjustNReverse = reverse . map (+ 33)
        group = (adjustNReverse .encodeWord32ToWord8s) group2Word32

-- {{{1 decode
-- | Decode data (lazy).
--
--   The input must not be enclosed in \<~ ~\>.
decode' :: String
    -> [Maybe Word8]
decode' [] = []
decode' ('z':cs) = (Just 0:Just 0:Just 0:Just 0:decode' cs)
decode' cs = let
        pad n = take n $ repeat $ Just 0
        dec :: [Maybe Word8] -> [Maybe Word8]
        dec l@[Just c1, Just c2] = take 1 . dec $ l ++ pad 3
        dec l@[Just c1, Just c2, Just c3] = take 2 . dec $ l ++ pad 2
        dec l@[Just c1, Just c2, Just c3, Just c4] = take 3 . dec $ l ++ pad 1
        dec l@[Just c1, Just c2, Just c3, Just c4, Just c5] = let
                adjRev = map (flip (-) 33) [c5, c4, c3, c2, c1]
                group2Word32 :: [Word8] -> Word32
                group2Word32 = foldl1 (+) . map (uncurry (*)) . zip (map (85 ^) [0..4]) . map fromIntegral
                word32ToGroup :: Word32 -> [Maybe Word8]
                word32ToGroup = map Just . map fromIntegral . reverse . take 4 . iterate (`div` 256)
            in (word32ToGroup . group2Word32) adjRev
        dec _ = [Nothing]
    in (dec . map (flip M.lookup decodeMap) $ take 5 cs) ++ decode' (drop 5 cs)

-- | Decode data (strict).
--
--   The input must not be enclosed in \<~ ~\>.
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
