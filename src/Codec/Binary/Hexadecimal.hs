-- |
-- Module    : Codec.Binary.Hexadecimal
-- Copyright : (c) 2009 Magnus Therning
-- License   : BSD3
--
-- Simple hexadecimal encoding.
--
-- Further documentation and information can be found at
-- <http://www.haskell.org/haskellwiki/Library/Data_encoding>.
module Codec.Binary.Hexadecimal
    ( encode
    , decode
    , decode'
    , chop
    , unchop
    ) where

import Data.Array
import Data.Bits
import Data.Char
import Data.Word
import qualified Data.Map as M

-- {{{1 enc/dec alist
_encMap = zip [0..] "0123456789ABCDEF"

encodeArray :: Array Word8 Char
encodeArray = array (0, 16) _encMap

decodeMap :: M.Map Char Word8
decodeMap = M.fromList [(snd i, fst i) | i <- _encMap]

-- {{{1
-- | Encode data.  Each nibble of the byte is converted into one character in
-- [0-9A-F].
encode :: [Word8]
    -> String
encode [] = ""
encode (o:os) = let
        hn = o `shiftR` 4
        ln = o .&. 0xf
    in (encodeArray ! hn) : (encodeArray ! ln) : (encode os)

-- {{{1 decode
-- | Decode data (lazy).
decode' :: String
    -> [Maybe Word8]
decode' = let
        dec [] = []
        dec (Just hn : Just ln : mos) = let
                o = hn `shiftL` 4 .|. ln
            in Just o : (dec mos)
        dec _ = [Nothing]
    in dec . map ((flip M.lookup decodeMap) . toUpper)

-- | Decode data (strict).
decode :: String -> Maybe [Word8]
decode = sequence . decode'

-- {{{1
-- | Chop up a string in parts.
chop :: Int     -- ^ length of individual lines
    -> String
    -> [String]
chop n "" = []
chop n s = let
        enc_len | n < 2 = 2
                | otherwise = n `div` 2 * 2
    in (take enc_len s) : chop n (drop enc_len s)

-- {{{1
-- | Concatenate the list of strings into one long string.
unchop :: [String] -> String
unchop = foldr (++) ""
