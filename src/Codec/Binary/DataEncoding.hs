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

-- | Data encoding module.
--
--   It exposes several instances of 'DataCodec', one for each data encoding
--   implemented in the library without causing the name clashing that would
--   result from importing the individual encoding modules.
--
module Codec.Binary.DataEncoding
    ( DataCodec
    , encode
    , decode
    , decode'
    , chop
    , unchop
    , base16
    , base32
    , base32Hex
    , base64
    , base64Url
    , base85
    , uu
    )
    where

import Data.Word

import qualified Codec.Binary.Base16 as Base16
import qualified Codec.Binary.Base32 as Base32
import qualified Codec.Binary.Base32Hex as Base32Hex
import qualified Codec.Binary.Base64 as Base64
import qualified Codec.Binary.Base64Url as Base64Url
import qualified Codec.Binary.Base85 as Base85
import qualified Codec.Binary.Uu as Uu

-- {{{1 DataCodec
-- | Used to group a specific data encoding's functions.
data DataCodec = DataCodec {
    encode :: [Word8] -> String,
    decode :: String -> Maybe [Word8],
    decode' :: String -> [Maybe Word8],
    chop :: Int -> String -> [String],
    unchop :: [String] -> String
}

-- {{{1 base16
-- | Base16 encoding, see "Codec.Binary.Base16" for more details on
--   the individual functions.
base16 :: DataCodec
base16 = DataCodec {
    encode=Base16.encode,
    decode=Base16.decode,
    decode'=Base16.decode',
    chop=Base16.chop,
    unchop=Base16.unchop
}

-- {{{1 base32
-- | Base32 encoding, see "Codec.Binary.Base32" for more details on
--   the individual functions.
base32 :: DataCodec
base32 = DataCodec {
    encode=Base32.encode,
    decode=Base32.decode,
    decode'=Base32.decode',
    chop=Base32.chop,
    unchop=Base32.unchop
}

-- {{{1 base32Hex
-- | Base32Hex encoding, see "Codec.Binary.Base32Hex" for more details
--   on the individual functions.
base32Hex :: DataCodec
base32Hex = DataCodec {
    encode=Base32Hex.encode,
    decode=Base32Hex.decode,
    decode'=Base32Hex.decode',
    chop=Base32Hex.chop,
    unchop=Base32Hex.unchop
}

-- {{{1 base64
-- | Base64 encoding, see "Codec.Binary.Base64" for more details on
--   the individual functions.
base64 :: DataCodec
base64 = DataCodec {
    encode=Base64.encode,
    decode=Base64.decode,
    decode'=Base64.decode',
    chop=Base64.chop,
    unchop=Base64.unchop
}

-- {{{1 base64Url
-- | Base64Url encoding, see "Codec.Binary.Base64Url" for more details
--   on the individual functions.
base64Url :: DataCodec
base64Url = DataCodec {
    encode=Base64Url.encode,
    decode=Base64Url.decode,
    decode'=Base64Url.decode',
    chop=Base64Url.chop,
    unchop=Base64Url.unchop
}

-- {{{1 base85
-- | Base85 encoding, see "Codec.Binary.Base85" for more details
--   on the individual functions.
base85 :: DataCodec
base85 = DataCodec {
    encode=Base85.encode,
    decode=Base85.decode,
    decode'=Base85.decode',
    chop=Base85.chop,
    unchop=Base85.unchop
}

-- {{{1 uu
-- | Uuencoding, see "Codec.Binary.Uu" for more details on the
--   individual functions.
uu :: DataCodec
uu = DataCodec {
    encode=Uu.encode,
    decode=Uu.decode,
    decode'=Uu.decode',
    chop=Uu.chop,
    unchop=Uu.unchop
}
