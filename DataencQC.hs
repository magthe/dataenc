{-
 - Copyright : (c) 2007 Magnus Therning
 - License   : BSD3
 -}

module DataencQC
    where

import Data.Maybe
import Data.Word
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2

import qualified Codec.Binary.Uu as Uu
import qualified Codec.Binary.Uu as Xx
import qualified Codec.Binary.Base85 as Base85
import qualified Codec.Binary.Base64 as Base64
import qualified Codec.Binary.Base64Url as Base64Url
import qualified Codec.Binary.Base32 as Base32
import qualified Codec.Binary.Base32Hex as Base32Hex
import qualified Codec.Binary.Base16 as Base16
import qualified Codec.Binary.Yenc as Yenc
import qualified Codec.Binary.Hexadecimal as Hex

-- {{{1 Arbitrary instances
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

-- {{{1 xxencode properties
prop_xxEncode ws = ws == (fromJust . Xx.decode . Xx.encode) ws
    where types = ws::[Word8]

prop_xxChop s = properLength s ==> s == (Xx.unchop . Xx.chop 45) s
    where
        types = s::String
        properLength s = length s `mod` 4 /= 1 -- property of xxencode guarantees this

prop_xxCombined ws = ws == (fromJust $ Xx.decode $ Xx.unchop $ Xx.chop 45 $ Xx.encode ws)
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

-- {{{1 yEncoding
prop_yencEncode ws = ws == (fromJust $ Yenc.decode $ Yenc.encode ws)
    where types = ws ::[Word8]

prop_yencChop ws = ws == (Yenc.unchop $ Yenc.chop 6 ws)
    where types = ws :: [Word8]

-- {{{1 hexadecimal
prop_hexEncode ws = ws == (fromJust $ Hex.decode $ Hex.encode ws)
    where types = ws :: [Word8]

prop_hexChop s = s == (Hex.unchop $ Hex.chop 6 s)
    where types = s :: String

-- {{{1 all the tests
allTests =
    [ testProperty "uuEncode" prop_uuEncode
    , testProperty "uuChop" prop_uuChop
    , testProperty "uuCombined" prop_uuCombined
    , testProperty "xxEncode" prop_xxEncode
    , testProperty "xxChop" prop_xxChop
    , testProperty "xxCombined" prop_xxCombined
    , testProperty "base85Encode" prop_base85Encode
    , testProperty "base85Chop" prop_base85Chop
    , testProperty "base64Encode" prop_base64Encode
    , testProperty "base64Chop" prop_base64Chop
    , testProperty "base64UrlEncode" prop_base64UrlEncode
    , testProperty "base64UrlChop" prop_base64UrlChop
    , testProperty "base32Encode" prop_base32Encode
    , testProperty "base32Chop" prop_base32Chop
    , testProperty "base32HexEncode" prop_base32HexEncode
    , testProperty "base32HexChop" prop_base32HexChop
    , testProperty "base16Encode" prop_base16Encode
    , testProperty "base16Chop" prop_base16Chop
    , testProperty "yencEncode" prop_yencEncode
    , testProperty "yencChop" prop_yencChop
    , testProperty "hexEncode" prop_hexEncode
    , testProperty "hexChop" prop_hexChop
    ]
