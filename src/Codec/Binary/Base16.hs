-- |
-- Module    : Codec.Binary.Base16
-- Copyright : (c) 2007 Magnus Therning
-- License   : BSD3
--
-- Implemented as specified in RFC 4648 (<http://tools.ietf.org/html/rfc4648>).
--
-- Further documentation and information can be found at
-- <http://www.haskell.org/haskellwiki/Library/Data_encoding>.
module Codec.Binary.Base16
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
-- | Incremental encoder function.
encodeInc :: EncIncData -> EncIncRes String
encodeInc EDone = EFinal []
encodeInc (EChunk os) = EPart (concat $ map toHex os) encodeInc

-- | Encode data.
encode :: [Word8] -> String
encode = encoder encodeInc

-- {{{1 decode
-- | Incremental decoder function.
decodeInc :: DecIncData String -> DecIncRes String
decodeInc d = dI [] d
    where
        dec2 cs = let
                ds = map (flip M.lookup decodeMap) cs
                es@[e1, e2] = map fromJust ds
                o = e1 `shiftL` 4 .|. e2
                allJust = and . map isJust
            in if allJust ds
                then Just o
                else Nothing

        dI [] Done = Final [] []
        dI lo Done = Fail [] lo
        dI lo (Chunk s) = doDec [] (lo ++ s)
            where
                doDec acc s'@(c1:c2:cs) = maybe
                    (Fail acc s')
                    (\ b -> doDec (acc ++ [b]) cs)
                    (dec2 [c1, c2])
                doDec acc s = Part acc (dI s)

-- | Decode data.
decode :: String -> Maybe [Word8]
decode = decoder decodeInc

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
    in take enc_len s : chop n (drop enc_len s)

-- {{{1 unchop
-- | Concatenate the strings into one long string.
unchop :: [String]
    -> String
unchop = foldr (++) ""
