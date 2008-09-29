{-
 - Copyright : (c) 2007 Magnus Therning
 - License   : BSD3
 -}

module Main
    where

import Data.Maybe
import Data.Word
import Test.QuickCheck

import qualified Codec.Binary.Uu as Uu
import qualified Codec.Binary.Base85 as Base85
import qualified Codec.Binary.Base64 as Base64
import qualified Codec.Binary.Base64Url as Base64Url
import qualified Codec.Binary.Base32 as Base32
import qualified Codec.Binary.Base32Hex as Base32Hex
import qualified Codec.Binary.Base16 as Base16

-- {{{1 Arbitrary instances
instance Arbitrary Char where
    arbitrary = do
        n <- choose ((fromEnum (minBound::Char))::Int,
                (fromEnum (maxBound::Char))::Int)
        return $ toEnum n

instance Arbitrary Word8 where
    arbitrary = do
        n <- choose ((fromIntegral (minBound::Word8))::Int,
                (fromIntegral (maxBound::Word8))::Int)
        return $ fromIntegral n

-- {{{1 uuencode properties
prop_uuEncode ws = ws == (fromJust . Uu.decode . Uu.encode) ws
    where types = ws::[Word8]

prop_uuChop s = properLength s ==> s == (Uu.unchop . Uu.chop 45) s
    where
        types = s::String
        properLength s = length s `mod` 4 /= 1 -- property of uuencode guarantees this

prop_uuCombined ws = ws == (fromJust $ Uu.decode $ Uu.unchop $ Uu.chop 45 $ Uu.encode ws)
    where types = ws::[Word8]

-- {{{1 base85 properties
prop_base85Encode ws = ws == (fromJust $ Base85.decode $ Base85.encode ws)
    where types = ws::[Word8]

prop_base85Chop s = s == (Base85.unchop $ Base85.chop 20 s)
    where types = s::String

-- {{{1 base64 properties
prop_base64Encode ws = ws == (fromJust $ Base64.decode $ Base64.encode ws)
    where types = ws::[Word8]

prop_base64Chop s = s == (Base64.unchop $ Base64.chop 4 s)
    where types = s::String

-- {{{1 base64url properties
prop_base64UrlEncode ws = ws == (fromJust $ Base64Url.decode $ Base64Url.encode ws)
    where types = ws::[Word8]

prop_base64UrlChop s = s == (Base64Url.unchop $ Base64Url.chop 64 s)
    where types = s::String

-- {{{1 base32
prop_base32Encode ws = ws == (fromJust $ Base32.decode $ Base32.encode ws)
    where types = ws::[Word8]

prop_base32Chop s = s == (Base32.unchop $ Base32.chop 8 s)
    where types = s::String

-- {{{1 base32hex
prop_base32HexEncode ws = ws == (fromJust $ Base32Hex.decode $ Base32Hex.encode ws)
    where types = ws::[Word8]

prop_base32HexChop s = s == (Base32Hex.unchop $ Base32Hex.chop 64 s)
    where types = s::String

-- {{{1 base16
prop_base16Encode ws = ws == (fromJust $ Base16.decode $ Base16.encode ws)
    where types = ws::[Word8]

prop_base16Chop s = s == (Base16.unchop $ Base16.chop 6 s)
    where types = s::String

-- {{{1 main
main = do
    quickCheck prop_uuEncode
    quickCheck prop_uuChop
    quickCheck prop_uuCombined
    quickCheck prop_base85Encode
    quickCheck prop_base85Chop
    quickCheck prop_base64Encode
    quickCheck prop_base64Chop
    quickCheck prop_base64UrlEncode
    quickCheck prop_base64UrlChop
    quickCheck prop_base32Encode
    quickCheck prop_base32Chop
    quickCheck prop_base32HexEncode
    quickCheck prop_base32HexChop
    quickCheck prop_base16Encode
    quickCheck prop_base16Chop
