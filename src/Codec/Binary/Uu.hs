-- |
-- Module    : Codec.Binary.Uu
-- Copyright : (c) 2007 Magnus Therning
-- License   : BSD3
--
-- Uuencoding is notoriously badly specified.  This implementation is
-- compatible with the GNU Sharutils
-- (<http://www.gnu.org/software/sharutils/>).
--
-- Further documentation and information can be found at
-- <http://www.haskell.org/haskellwiki/Library/Data_encoding>.
module Codec.Binary.Uu
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

import Control.Monad
import Data.Array
import Data.Bits
import Data.Maybe
import Data.Word
import qualified Data.Map as M

-- {{{1 enc/dec map
_encMap = zip [0..] "`!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"

-- {{{1 encodeArray
encodeArray :: Array Word8 Char
encodeArray = array (0, 64) _encMap

-- {{{1 decodeMap
decodeMap :: M.Map Char Word8
decodeMap = M.fromList [(snd i, fst i) | i <- _encMap]

-- {{{1 encode
-- | Incremental encoder function.
encodeInc :: EncIncData -> EncIncRes String
encodeInc e = eI [] e
    where
        enc3 [o1, o2, o3] = map (encodeArray !) [i1, i2, i3, i4]
            where
                i1 = o1 `shiftR` 2
                i2 = (o1 `shiftL` 4 .|. o2 `shiftR` 4) .&. 0x3f
                i3 = (o2 `shiftL` 2 .|. o3 `shiftR` 6) .&. 0x3f
                i4 = o3 .&. 0x3f

        eI [] EDone = EFinal []
        eI [o1] EDone = EFinal $ take 2 $ enc3 [o1, 0, 0]
        eI [o1, o2] EDone = EFinal $ take 3 $ enc3 [o1, o2, 0]
        eI lo (EChunk bs) = doEnc [] (lo ++ bs)
            where
                doEnc acc (o1:o2:o3:os) = doEnc (acc ++ enc3 [o1, o2, o3]) os
                doEnc acc os = EPart acc (eI os)

-- | Encode data.
encode :: [Word8] -> String
encode = encoder encodeInc

-- {{{1 decode
-- | Incremental decoder function.
decodeInc :: DecIncData String -> DecIncRes String
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

        dI [] DDone = DFinal [] []
        dI lo@[c1, c2] DDone = maybe
            (DFail [] lo)
            (\ bs -> DFinal (take 1 bs) [])
            (dec4 [c1, c2, '`', '`'])
        dI lo@[c1, c2, c3] DDone = maybe
            (DFail [] lo)
            (\ bs -> DFinal (take 2 bs) [])
            (dec4 [c1, c2, c3, '`'])
        dI lo DDone = DFail [] lo
        dI lo (DChunk s) = doDec [] (lo ++ s)
            where
                doDec acc s'@(c1:c2:c3:c4:cs) = maybe
                    (DFail acc s')
                    (\ bs -> doDec (acc ++ bs) cs)
                    (dec4 [c1, c2, c3, c4])
                doDec acc s' = DPart acc (dI s')

-- | Decode data.
decode :: String -> Maybe [Word8]
decode = decoder decodeInc

-- {{{1 chop
-- | Chop up a string in parts.  Each string in the resulting list is prepended
--   with the length according to the uuencode \"specificiation\".
--
--   /Notes:/
--
--   * The length of the strings in the result will be @(n -1) `div` 4 * 4 +
--   1@.  The @-1@ comes from the need to prepend the length (which explains
--   the final @+1@).  Keeping it to a multiple of 4 means that strings
--   returned from 'encode' can be chopped without requiring any changes.
--
--   * The length of lines in GNU's sharutils is 61.
chop :: Int     -- ^ length (value should be in the range @[5..85]@)
    -> String
    -> [String]
chop n "" = []
chop n s = let
        enc_len | n < 5     = 4
                | n >= 85    = 84
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
        singleUnchop (l : cs) = let
                act_len = fromIntegral $ decodeMap M.! l
                enc_len = case (act_len `divMod` 3) of
                    (n, 0) -> n * 4
                    (n, 1) -> n * 4 + 2
                    (n, 2) -> n * 4 + 3
            in take enc_len cs
    in foldr ((++) . singleUnchop) "" ss
