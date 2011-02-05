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
data EncIncData = EChunk [Word8] | EDone
data EncIncRes = EPart String (EncIncData -> EncIncRes) | EFinal String

encodeInc e = eI [] e
    where
        enc4 [0, 0, 0, 0] = "z"
        enc4 [20, 20, 20, 20] = "y"
        enc4 os@[o1, o2, o3, o4] = map (encodeArray !) group
            where
                group2Word32 = foldl (\ a b -> a `shiftL` 8 + fromIntegral b) 0 os
                encodeWord32ToWord8s :: Word32 -> [Word8]
                encodeWord32ToWord8s =
                    map (fromIntegral . (`mod` 85)) . take 5 . iterate (`div` 85)
                adjustNReverse = reverse . map (+ 33)
                group = (adjustNReverse .encodeWord32ToWord8s) group2Word32

        eI [] EDone = EFinal []
        eI [o1] EDone = EFinal (take 2 cs)
            where
                cs = enc4 [o1, 0, 0, 1]
        eI [o1, o2] EDone = EFinal (take 3 cs)
            where
                cs = enc4 [o1, o2, 0, 1]
        eI [o1, o2, o3] EDone = EFinal (take 4 cs)
            where
                cs = enc4 [o1, o2, o3, 1]
        eI lo (EChunk bs) = doEnc [] (lo ++ bs)
            where
                doEnc acc (o1:o2:o3:o4:os) = doEnc (acc ++ enc4 [o1, o2, o3, o4]) os
                doEnc acc os = EPart acc (eI os)

-- | Encode data.
--
--   The result will not be enclosed in \<~ ~\>.
encode :: [Word8]
    -> String
encode bs = case encodeInc (EChunk bs) of
    EPart r1 f -> case f EDone of
        EFinal r2 -> r1 ++ r2

-- {{{1 decode
decodeInc :: DecIncData String -> DecIncRes String
decodeInc d = dI [] d
    where
        dec5 cs = let
                ds = map (flip M.lookup decodeMap) cs
                es@[e1, e2, e3, e4, e5] = map fromJust ds
                adjRev = map (\ i -> i - 33) [e5, e4, e3, e2, e1]
                group2Word32 = foldl1 (+) . zipWith (*) (map (85 ^) [0..4]) . map fromIntegral
                word32ToGroup :: Word32 -> [Word8]
                word32ToGroup = map fromIntegral . reverse . take 4 . iterate (`div` 256)
                allJust = and . map isJust
            in if allJust ds
                then Just $ word32ToGroup $ group2Word32 adjRev
                else Nothing

        dI lo (Chunk s) = doDec [] (lo ++ s)
        dI [] Done = Final [] []
        dI cs@[c1, c2] Done = case doDec [] (cs ++ "uuu") of
                (Part r _) -> Final (take 1 r) []
                f -> f
        dI cs@[c1, c2, c3] Done = case doDec [] (cs ++ "uu") of
                (Part r _) -> Final (take 2 r) []
                f -> f
        dI cs@[c1, c2, c3, c4] Done = case doDec [] (cs ++ "u") of
                (Part r _) -> Final (take 3 r) []
                f -> f
        dI lo Done = Fail [] lo

        doDec acc ('z':cs) = doDec (acc ++ [0, 0, 0, 0]) cs
        doDec acc ('y':cs) = doDec (acc ++ [20, 20, 20, 20]) cs
        doDec acc s@(c1:c2:c3:c4:c5:cs) = maybe
            (Fail acc s)
            (\ bs -> doDec (acc ++ bs) cs)
            (dec5 [c1, c2, c3, c4, c5])
        doDec acc cs = Part acc (dI cs)

-- | Decode data (strict).
--
--   The input must not be enclosed in \<~ ~\>.
decode :: String
    -> Maybe [Word8]
decode = decoder decodeInc

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
    in take enc_len s : chop n (drop enc_len s)

-- {{{1 unchop
-- | Concatenate the strings into one long string.
unchop :: [String]
    -> String
unchop = foldr (++) ""
