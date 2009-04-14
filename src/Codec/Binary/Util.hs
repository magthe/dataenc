-- |
-- Module    : Codec.Binary.Util
-- Copyright : (c) 2009 Magnus Therning
-- License   : BSD3
--
-- Utility functions used in the other module.
module Codec.Binary.Util
    ( toHex
    , fromHex
    ) where

import Data.Array
import Data.Bits
import Data.Char
import Data.Word
import qualified Data.Map as M

-- {{{1 hex enc/dec assoc list and maps
hexEncMap = zip [0..] "0123456789ABCDEF"

hexEncodeArray :: Array Word8 Char
hexEncodeArray = array (0, 16) hexEncMap

hexDecodeMap :: M.Map Char Word8
hexDecodeMap = M.fromList [(b, a) | (a, b) <- hexEncMap]

-- {{{1 toHex
toHex :: Word8 -> String
toHex o = let
        hn = o `shiftR` 4
        ln = o .&. 0xf
    in [hexEncodeArray ! hn, hexEncodeArray ! ln]

-- {{{1 fromHex
fromHex :: String -> Maybe Word8
fromHex = let
        dec [Just hn, Just ln] = let
                o = hn `shiftL` 4 .|. ln
            in Just o
        dec _ = Nothing
    in dec . map ((flip M.lookup hexDecodeMap) . toUpper)
