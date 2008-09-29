-- |
-- Module    : Codec.Binary.Base32Hex
-- Copyright : (c) 2007 Magnus Therning
-- License   : BSD3
--
-- Implemented as specified in RFC 4648
-- (<http://tools.ietf.org/html/rfc4648>).
--
-- Further documentation and information can be found at
-- <http://www.haskell.org/haskellwiki/Library/Data_encoding>.
module Codec.Binary.Base32Hex
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

import qualified Codec.Binary.Base32 as Base32

-- {{{1 enc/dec map
_encMap =
    [ (0, '0'), (1, '1'), (2, '2'), (3, '3'), (4, '4')
    , (5, '5'), (6, '6'), (7, '7'), (8, '8'), (9, '9')
    , (10, 'A'), (11, 'B'), (12, 'C'), (13, 'D'), (14, 'E')
    , (15, 'F'), (16, 'G'), (17, 'H'), (18, 'I'), (19, 'J')
    , (20, 'K'), (21, 'L'), (22, 'M'), (23, 'N'), (24, 'O')
    , (25, 'P'), (26, 'Q'), (27, 'R'), (28, 'S'), (29, 'T')
    , (30, 'U'), (31, 'V') ]

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
        pad n = take n . repeat $ Just 0
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
