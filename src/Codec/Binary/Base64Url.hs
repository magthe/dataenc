-- |
-- Module    : Codec.Binary.Base64Url
-- Copyright : (c) 2007 Magnus Therning
-- License   : BSD3
--
-- Implemented as specified in RFC 4648 (<http://tools.ietf.org/html/rfc4648>).
--
-- Further documentation and information can be found at
-- <http://www.haskell.org/haskellwiki/Library/Data_encoding>.
module Codec.Binary.Base64Url
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

import Data.Maybe
import Data.Word
import Data.Bits
import Data.Array
import qualified Data.Map as M

import qualified Codec.Binary.Base64 as Base64

-- {{{1 enc/dec map
_encMap =
    [ (0, 'A'), (1, 'B'), (2, 'C'), (3, 'D'), (4, 'E')
    , (5, 'F') , (6, 'G'), (7, 'H'), (8, 'I'), (9, 'J')
    , (10, 'K'), (11, 'L'), (12, 'M'), (13, 'N'), (14, 'O')
    , (15, 'P'), (16, 'Q'), (17, 'R'), (18, 'S'), (19, 'T')
    , (20, 'U'), (21, 'V'), (22, 'W'), (23, 'X'), (24, 'Y')
    , (25, 'Z'), (26, 'a'), (27, 'b'), (28, 'c'), (29, 'd')
    , (30, 'e'), (31, 'f'), (32, 'g'), (33, 'h'), (34, 'i')
    , (35, 'j'), (36, 'k'), (37, 'l'), (38, 'm'), (39, 'n')
    , (40, 'o'), (41, 'p'), (42, 'q'), (43, 'r'), (44, 's')
    , (45, 't'), (46, 'u'), (47, 'v'), (48, 'w'), (49, 'x')
    , (50, 'y'), (51, 'z'), (52, '0'), (53, '1'), (54, '2')
    , (55, '3'), (56, '4'), (57, '5'), (58, '6'), (59, '7')
    , (60, '8'), (61, '9'), (62, '-'), (63, '_') ]

-- {{{1 encodeArray
encodeArray :: Array Word8 Char
encodeArray = array (0, 64) _encMap

-- {{{1 decodeMap
decodeMap :: M.Map Char Word8
decodeMap  = M.fromList [(snd i, fst i) | i <- _encMap]

-- {{{1 encode
-- | Incremental encoder function.
encodeInc :: EncIncData -> EncIncRes String
encodeInc e = eI [] e
    where
        enc3 [o1, o2, o3] = cs
            where
                i1 = o1 `shiftR` 2
                i2 = (o1 `shiftL` 4 .|. o2 `shiftR` 4) .&. 0x3f
                i3 = (o2 `shiftL` 2 .|. o3 `shiftR` 6) .&. 0x3f
                i4 = o3 .&. 0x3f
                cs = map (encodeArray !) [i1, i2, i3, i4]

        eI [] EDone = EFinal []
        eI [o1] EDone = EFinal (take 2 cs ++ "==")
            where cs = enc3 [o1, 0, 0]
        eI [o1, o2] EDone = EFinal (take 3 cs ++ "=")
            where cs = enc3 [o1, o2, 0]
        eI lo (EChunk bs) = doEnc [] (lo ++ bs)
            where
                doEnc acc (o1:o2:o3:os) = doEnc (acc ++ enc3 [o1, o2, o3]) os
                doEnc acc os = EPart acc (eI os)

-- | Encode data.
encode :: [Word8] -> String
encode = encoder encodeInc

-- {{{1 decode
-- | Incremental encoder function.
decodeInc :: DecIncData String -> DecIncRes String
decodeInc d = dI [] d
    where
        dec4 cs = let
                ds = map (flip M.lookup decodeMap) cs
                es@[e1, e2, e3, e4] = map fromJust ds
                o1 = e1 `shiftL` 2 .|. e2 `shiftR` 4
                o2 = e2 `shiftL` 4 .|. e3 `shiftR` 2
                o3 = e3 `shiftL` 6 .|. e4
                allJust = and . map isJust
            in if allJust ds
                then Just [o1, o2, o3]
                else Nothing

        dI [] (DDone) = DFinal [] []
        dI lo (DDone) = DFail [] lo
        dI lo (DChunk s) = doDec [] (lo ++ s)
            where
                doDec acc s@(c1:c2:'=':'=':cs) = maybe
                    (DFail acc s)
                    (\ bs -> DFinal (acc ++ take 1 bs) cs)
                    (dec4 [c1, c2, 'A', 'A'])
                doDec acc s@(c1:c2:c3:'=':cs) = maybe
                    (DFail acc s)
                    (\ bs -> DFinal (acc ++ take 2 bs) cs)
                    (dec4 [c1, c2, c3, 'A'])
                doDec acc s@(c1:c2:c3:c4:cs) = maybe
                    (DFail acc s)
                    (\ bs -> doDec (acc ++ bs) cs)
                    (dec4 [c1, c2, c3, c4])
                doDec acc s = DPart acc (dI s)

-- | Decode data.
decode :: String -> Maybe [Word8]
decode = decoder decodeInc

-- {{{1 chop
-- | Chop up a string in parts.
--
--   See 'Base64.chop' in "Base64" for more details.
chop :: Int     -- ^ length of individual lines
    -> String
    -> [String]
chop = Base64.chop

-- {{{1 unchop
-- | Concatenate the strings into one long string.
--
--   See 'Base64.unchop' in "Codec.Binary.Base64" for more details.
unchop :: [String]
    -> String
unchop = Base64.unchop
