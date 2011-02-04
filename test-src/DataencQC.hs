{-# OPTIONS_GHC -XTemplateHaskell #-}

{-
 - Copyright : (c) 2007 Magnus Therning
 - License   : BSD3
 -}

module DataencQC
    where

import Test.Framework.TH

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
import qualified Codec.Binary.QuotedPrintable as QP
import qualified Codec.Binary.PythonString as Py
import qualified Codec.Binary.Url as Url

-- {{{1 Arbitrary instances
instance Arbitrary Word8 where
    arbitrary = do
        n <- choose (fromIntegral (minBound::Word8) :: Int,
                fromIntegral (maxBound::Word8) :: Int)
        return $ fromIntegral n

-- {{{1 uuencode properties
prop_uuEncode ws = ws == (fromJust . Uu.decode . Uu.encode) ws
    where types = ws::[Word8]

prop_uuChop n ws = s == (Uu.unchop . Uu.chop n) s
    where
        types = (n :: Int, ws::[Word8])
        s = Uu.encode ws

prop_uuCombined n ws = ws == fromJust (Uu.decode $ Uu.unchop $ Uu.chop n $ Uu.encode ws)
    where types = (n::Int, ws::[Word8])

-- {{{1 xxencode properties
prop_xxEncode ws = ws == (fromJust . Xx.decode . Xx.encode) ws
    where types = ws::[Word8]

prop_xxChop n ws = s == (Xx.unchop . Xx.chop n) s
    where
        types = (n:: Int, ws::[Word8])
        s = Xx.encode ws

prop_xxCombined n ws = ws == fromJust (Xx.decode $ Xx.unchop $ Xx.chop n $ Xx.encode ws)
    where types = (n::Int, ws::[Word8])

-- {{{1 base85 properties
prop_base85Encode ws = ws == fromJust (Base85.decode $ Base85.encode ws)
    where types = ws::[Word8]

prop_base85Chop n s = s == Base85.unchop (Base85.chop n s)
    where types = (n::Int, s::String)

-- {{{1 base64 properties
prop_base64Encode ws = ws == fromJust (Base64.decode $ Base64.encode ws)
    where types = ws::[Word8]

prop_base64Chop n s = s == Base64.unchop (Base64.chop n s)
    where types = (n::Int, s::String)

-- {{{1 base64url properties
prop_base64UrlEncode ws = ws == fromJust (Base64Url.decode $ Base64Url.encode ws)
    where types = ws::[Word8]

prop_base64UrlChop n s = s == Base64Url.unchop (Base64Url.chop n s)
    where types = (n::Int, s::String)

-- {{{1 base32
prop_base32Encode ws = ws == fromJust (Base32.decode $ Base32.encode ws)
    where types = ws::[Word8]

prop_base32Chop n s = s == Base32.unchop (Base32.chop n s)
    where types = (n::Int, s::String)

-- {{{1 base32hex
prop_base32HexEncode ws = ws == fromJust (Base32Hex.decode $ Base32Hex.encode ws)
    where types = ws::[Word8]

prop_base32HexChop n s = s == Base32Hex.unchop (Base32Hex.chop n s)
    where types = (n::Int, s::String)

-- {{{1 base16
prop_base16Encode ws = ws == fromJust (Base16.decode $ Base16.encode ws)
    where types = ws::[Word8]

prop_base16Chop n s = s == Base16.unchop (Base16.chop n s)
    where types = (n::Int, s::String)

-- {{{1 yEncoding
prop_yencEncode ws = ws == fromJust (Yenc.decode $ Yenc.encode ws)
    where types = ws ::[Word8]

prop_yencChop n ws = ws == Yenc.unchop (Yenc.chop n ws)
    where types = (n::Int, ws :: [Word8])

-- {{{1 qp
prop_qpEncode ws = ws == fromJust (QP.decode $ QP.encode ws)
    where types = ws :: [Word8]

prop_qpChop n ws = s == (QP.unchop . QP.chop n) s
    where
        types = (n::Int, ws::[Word8])
        s = QP.encode ws

prop_qpCombined n ws = ws == fromJust (QP.decode $ QP.unchop $ QP.chop n $ QP.encode ws)
    where types = (n::Int, ws::[Word8])

-- {{{1 py
prop_pyEncode ws = ws == fromJust (Py.decode $ Py.encode ws)
    where types = ws :: [Word8]

prop_pyChop n s = s == Py.unchop (Py.chop n s)
    where types = (n :: Int, s :: String)

prop_pyCombined n ws = ws == fromJust (runAll ws)
    where runAll = Py.decode . Py.unchop . Py.chop n . Py.encode

-- {{{1 url
prop_urlEncode ws = ws == fromJust (Url.decode $ Url.encode ws)
    where types = ws :: [Word8]

prop_urlChop n s = s == Url.unchop (Url.chop n s)
    where types = (n :: Int, s :: String)

prop_urlCombined n ws = ws == fromJust (runAll ws)
    where runAll = Url.decode . Url.unchop . Url.chop n . Url.encode

-- {{{1 all the tests
allTests = $(testGroupGenerator)
