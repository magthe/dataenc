-- |
-- Module    : Codec.Binary.Yenc
-- Copyright : (c) 2007 Magnus Therning
-- License   : BSD3
--
-- Implementation based on the specification found at
-- <http://yence.sourceforge.net/docs/protocol/version1_3_draft.html>.
--
-- Further documentation and information can be found at
-- <http://www.haskell.org/haskellwiki/Library/Data_encoding>.
module Codec.Binary.Yenc
    ( encode
    , decode
    , decode'
    , chop
    , unchop
    ) where

import Data.Word

_criticalsIn = [0xd6, 0xe0, 0xe3, 0x13]
_equal = 0x3d

-- {{{1 encode
-- | Encode data.
encode :: [Word8]
    -> [Word8]
encode (d : ds)
    | d `elem` _criticalsIn = _equal : d + 106 : encode ds
    | otherwise = d + 42 : encode ds
encode _ = []

-- {{{1 decode
-- | Decode data (lazy).
decode' :: [Word8]
    -> [Maybe Word8]
decode' (0x3d : d : ds) = Just (d + 150) : decode' ds
decode' (d : ds) = Just (d + 214) : decode' ds
decode' _ = []

-- | Decode data (strict).
decode :: [Word8]
    -> Maybe [Word8]
decode = sequence . decode'

-- {{{1 chop
-- | Chop up a string in parts.
chop :: Int     -- ^ length of individual lines
    -> [Word8]
    -> [[Word8]]
chop _ [] = []
chop n ws = let
        _n = max n 1
        (p1, p2) = splitAt _n ws
    in
        if last p1 == _equal
            then (p1 ++ take 1 p2) : chop _n (drop 1 p2)
            else p1 : chop _n p2

-- {{{1 unchop
-- | Concatenate the strings into one long string.
unchop :: [[Word8]]
    -> [Word8]
unchop = concat
