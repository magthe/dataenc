-- |
-- Module    : Codec.Binary.Xx
-- Copyright : (c) 2007 Magnus Therning
-- License   : BSD3
--
-- Xxencoding is obsolete but still included for completeness.  Further
-- information on the encoding can be found at
-- <http://en.wikipedia.org/wiki/Xxencode>.  It should be noted that this
-- implementation performs no padding, due to the splitting up between encoding
-- and chopping.
--
-- Further documentation and information can be found at
-- <http://www.haskell.org/haskellwiki/Library/Data_encoding>.
module Codec.Binary.Xx
    ( encode
    , DecIncData(..)
    , DecIncRes(..)
    , decodeInc
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

-- {{{1 enc/dec map
_encMap = zip [0..] "+-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

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
encode = let
        pad n = replicate n 0
        enc [] = ""
        enc l@[o] = take 2 . enc $ l ++ pad 2
        enc l@[o1, o2] = take 3 . enc $ l ++ pad 1
        enc (o1 : o2 : o3 : os) = let
                i1 = o1 `shiftR` 2
                i2 = (o1 `shiftL` 4 .|. o2 `shiftR` 4) .&. 0x3f
                i3 = (o2 `shiftL` 2 .|. o3 `shiftR` 6) .&. 0x3f
                i4 = o3 .&. 0x3f
            in foldr ((:) . (encodeArray !)) "" [i1, i2, i3, i4] ++ enc os
    in enc

-- {{{1 decode
data DecIncData = Chunk String | Done
data DecIncRes = Part [Word8] (DecIncData -> DecIncRes) | Final [Word8] String | Fail [Word8] String

decodeInc :: DecIncData -> DecIncRes
decodeInc d = dI [] d
    where
        dec4 cs = let
                ds = map (flip M.lookup decodeMap) cs
                [e1, e2, e3, e4] = map fromJust ds
                o1 = e1 `shiftL` 2 .|. e2 `shiftR` 4
                o2 = e2 `shiftL` 4 .|. e3 `shiftR` 2
                o3 = e3 `shiftL` 6 .|. e4
                allJust = and . map isJust
            in if allJust ds
                then Just [o1, o2, o3]
                else Nothing

        dI [] Done = Final [] []
        dI lo@[c1, c2] Done = maybe
            (Fail [] lo)
            (\ bs -> Final (take 1 bs) [])
            (dec4 [c1, c2, '+', '+'])
        dI lo@[c1, c2, c3] Done = maybe
            (Fail [] lo)
            (\ bs -> Final (take 2 bs) [])
            (dec4 [c1, c2, c3, '+'])
        dI lo Done = Fail [] lo
        dI lo (Chunk s) = doDec [] (lo ++ s)
            where
                doDec acc s'@(c1:c2:c3:c4:cs) = maybe
                    (Fail acc s')
                    (\ bs -> doDec (acc ++ bs) cs)
                    (dec4 [c1, c2, c3, c4])
                doDec acc s' = Part acc (dI s')

-- | Decode data (strict).
decode :: String
    -> Maybe [Word8]
decode s = let
        d = decodeInc (Chunk s)
    in case d of
        Final da _ -> Just da
        Fail _ _ -> Nothing
        Part da f -> let
                d' = f Done
            in case d' of
                Final da' _ -> Just $ da ++ da'
                Fail _ _ -> Nothing
                Part _ _ -> Nothing -- should never happen

-- {{{1 chop
-- | Chop up a string in parts.  Each string in the resulting list is prepended
--   with the length according to the xxencode \"specificiation\".
--
--   /Notes:/
--
--   * The length of the strings in the result will be @(n -1) `div` 4 * 4 +
--   1@.  The @-1@ comes from the need to prepend the length (which explains
--   the final @+1@).  Keeping it to a multiple of 4 means that strings
--   returned from 'encode' can be chopped without requiring any changes.
chop :: Int     -- ^ length (value should be in the range @[5..85]@)
    -> String
    -> [String]
chop n "" = []
chop n s = let
        enc_len | n < 5     = 4
                | n >= 85   = 84
                | otherwise = min 64 $ (n - 1) `div` 4 * 4
        enc_line = take enc_len s
        act_len = fromIntegral $ case (length enc_line `divMod` 4) of
            (l, 0) -> l * 3
            (l, 2) -> l * 3 + 1
            (l, 3) -> l * 3 + 2
        len = (encodeArray ! act_len)
    in (len : enc_line) : chop n (drop enc_len s)

-- {{{1 unchop
-- | Concatenate the strings into one long string.  Each string is assumed to
--   be prepended with the length according to the xxencode specification.
unchop :: [String]
    -> String
unchop ss = let
        singleUnchop (l : cs) = let
                act_len = fromIntegral $ decodeMap M.! l
                enc_len = case (act_len `divMod` 3) of
                    (n, 0) -> n * 4
                    (n, 1) -> n * 4 + 2
                    (n, 2) -> n * 4 + 3
            in take enc_len cs
    in foldr ((++) . singleUnchop) "" ss
