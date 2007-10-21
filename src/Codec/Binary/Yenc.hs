{- Copyright © 2007 Magnus Therning
 -
 - This file is part of dataenc.
 -
 - Dataenc is free software: you can redistribute it and/or modify it under
 - the terms of the GNU Lesser General Public License as published by the
 - Free Software Foundation, either version 3 of the License, or (at your
 - option) any later version.
 -
 - Dataenc is distributed in the hope that it will be useful, but WITHOUT
 - ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 - FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 - License for more details.
 -
 - You should have received a copy of the GNU Lesser General Public License
 - along with dataenc.  If not, see <http://www.gnu.org/licenses/>
 -}

-- | yEncoding module.
--
--   Implementation based on the specification found at
--   <http://yence.sourceforge.net/docs/protocol/version1_3_draft.html>.
module Codec.Binary.YEnc
    ( encode
    , decode
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
-- | Decode data.
decode :: String
    -> [Word8]
decode _ = []

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
