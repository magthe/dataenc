-- |
-- Module    : Codec.Binary.Util
-- Copyright : (c) 2009 Magnus Therning
-- License   : BSD3
--
-- Utility functions used in the other module.
module Codec.Binary.Util
    ( toHex
    , fromHex
    , EncIncData(..)
    , EncIncRes(..)
    , DecIncData(..)
    , DecIncRes(..)
    , encoder
    , decoder
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
    in dec . map (flip M.lookup hexDecodeMap . toUpper)

-- {{{1 incremental coding
-- | Data type for the incremental encoding functions.
data EncIncData = EChunk [Word8] -- ^ a chunk of data to be encoded
    | EDone -- ^ the signal to the encoder that the stream of data is ending

-- | Data type for the result of calling the incremental encoding functions.
data EncIncRes i = EPart i (EncIncData -> EncIncRes i) -- ^ a partial result together with the continuation to use for further encoding
    | EFinal i -- ^ the final result of encoding (the response to 'EDone')

encoder f os = case f (EChunk os) of
    EPart r1 f' -> case f' EDone of
        EFinal r2 -> r1 ++ r2

-- | Data type for the incremental decoding functions.
data DecIncData i = DChunk i -- ^ a chunk of data to be decoded
    | DDone -- ^ the signal to the decoder that the stream of data is ending

-- | Data type for the result of calling the incremental encoding functions.
data DecIncRes i = DPart [Word8] (DecIncData i -> DecIncRes i) -- ^ a partial result together with the continuation to user for further decoding
    | DFinal [Word8] i -- ^ the final result of decoding (the response to 'DDone')
    | DFail [Word8] i -- ^ a partial result for a failed decoding, together with the remainder of the data passed in so far

decoder :: (DecIncData i -> DecIncRes i) -> i -> Maybe [Word8]
decoder f s = let
        d = f (DChunk s)
    in case d of
        DFinal da _ -> Just da
        DFail _ _ -> Nothing
        DPart da f -> let
                d' = f DDone
            in case d' of
                DFinal da' _ -> Just $ da ++ da'
                DFail _ _ -> Nothing
                DPart _ _ -> Nothing -- should never happen
