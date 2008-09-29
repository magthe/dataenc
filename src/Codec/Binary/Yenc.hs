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
module Codec.Binary.YEnc
    ( encode
    , decode
    , decode'
    , chop
    , unchop
    ) where

import Data.Word.Word8

-- {{{1 encode
-- | Encode data.
encode :: [Word8]
    -> String
encode _ = ""

-- {{{1 decode
-- | Decode data (lazy).
decode :: String
    -> [Maybe Word8]
decode _ = []

-- | Decode data (strict).
decode :: String
    -> Maybe [Word8]
decode _ = Just []

-- {{{1 chop
-- | Chop up a string in parts.
chop :: Int     -- ^ length of individual lines
    -> String
    -> [String]
chop _ _ = []

-- {{{1 unchop
-- | Concatenate the strings into one long string.
unchop :: [String]
    -> String
unchop _ = ""
