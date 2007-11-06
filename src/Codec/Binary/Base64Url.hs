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

-- | Base64Url module.
--
--   Implemented as specified in RFC 4648
--   (<http://tools.ietf.org/html/rfc4648>).
module Codec.Binary.Base64Url
    ( encode
    , decode
    , decode'
    , chop
    , unchop
    ) where

import Data.Maybe
import Data.Word

import qualified Codec.Binary.Base64 as Base64

-- {{{1 encode
-- | Encode data.
encode :: [Word8]
    -> String
encode os = let
        swapUrlChar c = case c of
            '+' -> '-'
            '/' -> '_'
            _ -> c
    in map swapUrlChar $ Base64.encode os

-- {{{1 decode
-- | Decode data (lazy).
decode' :: String
    -> [Maybe Word8]
decode' = let
        unSwapUrlChar c = case c of
            '-' -> '+'
            '_' -> '/'
            _ -> c
    in Base64.decode' . map unSwapUrlChar

-- | Decode data (strict).
decode :: String
    -> Maybe [Word8]
decode = sequence . decode'

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
