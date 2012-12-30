{-# OPTIONS_GHC -XTemplateHaskell #-}
{-
 - Copyright : (c) 2007 Magnus Therning
 - License   : BSD3
 -}

module DataencUT
    where

import Test.HUnit
import Control.Monad
import System.Exit
import Data.Maybe
import qualified Test.Framework.Providers.API as TFAPI
import Test.Framework.TH
import Test.Framework.Providers.HUnit

import Codec.Binary.DataEncoding
import qualified Codec.Binary.Yenc as Yenc

-- {{{1 checkAssertions
checkAssertions (suite, desc, enc, dec, codec) = do
    enc @=? encode codec dec
    dec @=? fromJust (decode codec enc)

-- {{{1 uuencode tests
uuTestData =
    [ ("uu", "empty", "", [], uu)
    , ("uu", "\\0", "``", [0], uu)
    , ("uu", "\\255", "_P", [255], uu)
    , ("uu", "AA", "04$", [65, 65], uu)
    , ("uu", "AAA", "04%!", [65, 65, 65], uu)
    , ("uu", "AAAA", "04%!00", [65, 65, 65, 65], uu)
    , ("uu", "Example", "17AA;7!L90", [69,120,97,109,112,108,101], uu)
    ]
case_uuTests = mapM_ checkAssertions uuTestData

case_uuTests2 = do
    "EI2" @=? unchop uu (chop uu 1 "EI2")
    "EI3-" @=? unchop uu (chop uu 1 "EI3-")
    "EI3-EE" @=? unchop uu (chop uu 1 "EI3-EE")
    [0..255] @=? fromJust (decode uu $ unchop uu $ chop uu 1 $ encode uu [0..255])
    [0..255] @=? fromJust (decode uu $ unchop uu $ chop uu 61 $ encode uu [0..255])
    [0..255] @=? fromJust (decode uu $ unchop uu $ chop uu 100 $ encode uu [0..255])

case_uuTestsFail = do
    Nothing @=? decode uu "A"
    Nothing @=? decode uu "aa"

-- {{{1 xxencode tests
xxTestData =
    [ ("xx", "empty", "", [], xx)
    , ("xx", "\\0", "++", [0], xx)
    , ("xx", "\\255", "zk", [255], xx)
    , ("xx", "AA", "EI2", [65, 65], xx)
    , ("xx", "AAA", "EI3-", [65, 65, 65], xx)
    , ("xx", "AAAA", "EI3-EE", [65, 65, 65, 65], xx)
    , ("xx", "Example", "FLVVPL-gNE", [69,120,97,109,112,108,101], xx)
    ]
case_xxTest = mapM_ checkAssertions xxTestData

case_xxTests2 = do
    "EI2" @=? unchop xx (chop xx 1 "EI2")
    "EI3-" @=? unchop xx (chop xx 1 "EI3-")
    "EI3-EE" @=? unchop xx (chop xx 1 "EI3-EE")
    [0..255] @=? fromJust (decode xx $ unchop xx $ chop xx 1 $ encode xx [0..255])
    [0..255] @=? fromJust (decode xx $ unchop xx $ chop xx 61 $ encode xx [0..255])
    [0..255] @=? fromJust (decode xx $ unchop xx $ chop xx 100 $ encode xx [0..255])

case_xxTestsFail = do
    Nothing @=? decode xx "A"
    Nothing @=? decode xx "''"

-- {{{1 base85 tests
base85TestData =
    [ ("base85", "empty", "", [], base85)
    , ("base85", "f", "Ac", [102], base85)
    , ("base85", "fo", "Ao@", [102,111], base85)
    , ("base85", "foo", "AoDS", [102,111,111], base85)
    , ("base85", "foob", "AoDTs", [102,111,111,98], base85)
    , ("base85", "fooba", "AoDTs@/", [102,111,111,98,97], base85)
    , ("base85", "foobar", "AoDTs@<)", [102,111,111,98,97,114], base85)
    , ("base85", "\0", "!!", [0], base85)
    , ("base85", "foob\0\0\0\0ar", "AoDTszEW", [102,111,111,98,0,0,0,0,114], base85)
    , ("base85", "Example", "7<i6XE,9(", [69,120,97,109,112,108,101], base85)
    , ("base85", "zeros", "z", [0, 0, 0, 0], base85)
    , ("base85", "spaces", "y", [0x20, 0x20, 0x20, 0x20], base85)
    ]
case_base85Tests = mapM_ checkAssertions base85TestData

case_base85TestsFail = do
    Nothing @=? decode base85 "A"
    Nothing @=? decode base85 "!z"
    Nothing @=? decode base85 "!z!"
    Nothing @=? decode base85 "!z!z"

-- {{{1 base64 tests
base64TestData =
    [ ("base64", "empty", "", [], base64)
    , ("base64", "f", "Zg==", [102], base64)
    , ("base64", "fo", "Zm8=", [102,111], base64)
    , ("base64", "foo", "Zm9v", [102,111,111], base64)
    , ("base64", "foob", "Zm9vYg==", [102,111,111,98], base64)
    , ("base64", "fooba", "Zm9vYmE=", [102,111,111,98,97], base64)
    , ("base64", "foobar", "Zm9vYmFy", [102,111,111,98,97,114], base64)
    , ("base64", "\0", "AA==", [0], base64)
    , ("base64", "\255", "/w==", [255], base64)
    , ("base64", "Example", "RXhhbXBsZQ==", [69,120,97,109,112,108,101], base64)
    ]
case_base64Tests = mapM_ checkAssertions base64TestData

case_base64TestsFail = do
    Nothing @=? decode base64 "A"
    Nothing @=? decode base64 "!!"

-- {{{1 base64url tests
base64UrlTestData =
    [ ("base64url", "empty", "", [], base64Url)
    , ("base64url", "\0", "AA==", [0], base64Url)
    , ("base64url", "\255", "_w==", [255], base64Url)
    , ("base64url", "Example", "RXhhbXBsZQ==", [69,120,97,109,112,108,101], base64Url)
    ]
case_base64UrlTests = mapM_ checkAssertions base64UrlTestData

-- {{{1 base32 tests
base32TestData =
    [ ("base32", "empty", "", [], base32)
    , ("base32", "f", "MY======", [102], base32)
    , ("base32", "fo", "MZXQ====", [102,111], base32)
    , ("base32", "foo", "MZXW6===", [102,111,111], base32)
    , ("base32", "foob", "MZXW6YQ=", [102,111,111,98], base32)
    , ("base32", "fooba", "MZXW6YTB", [102,111,111,98,97], base32)
    , ("base32", "foobar", "MZXW6YTBOI======", [102,111,111,98,97,114], base32)
    ]
case_base32Tests = mapM_ checkAssertions base32TestData

case_base32TestsFail = do
    Nothing @=? decode base32 "A"
    Nothing @=? decode base32 "gh"

-- {{{1 base32hex tests
base32HexTestData =
    [ ("base32hex", "empty", "", [], base32Hex)
    , ("base32hex", "f", "CO======", [102], base32Hex)
    , ("base32hex", "fo", "CPNG====", [102,111], base32Hex)
    , ("base32hex", "foo", "CPNMU===", [102,111,111], base32Hex)
    , ("base32hex", "foob", "CPNMUOG=", [102,111,111,98], base32Hex)
    , ("base32hex", "fooba", "CPNMUOJ1", [102,111,111,98,97], base32Hex)
    , ("base32hex", "foobar", "CPNMUOJ1E8======", [102,111,111,98,97,114], base32Hex)
    ]
case_base32HexTests = mapM_ checkAssertions base32HexTestData

case_base32HexTestsFail = do
    Nothing @=? decode base32Hex "A"
    Nothing @=? decode base32Hex "gh"

-- {{{1 base16 (hex)
base16TestData =
    [ ("base16", "empty", "", [], base16)
    , ("base16", "f", "66", [102], base16)
    , ("base16", "fo", "666F", [102,111], base16)
    , ("base16", "foo", "666F6F", [102,111,111], base16)
    , ("base16", "foob", "666F6F62", [102,111,111,98], base16)
    , ("base16", "fooba", "666F6F6261", [102,111,111,98,97], base16)
    , ("base16", "foobar", "666F6F626172", [102,111,111,98,97,114], base16)
    ]
case_base16Tests = mapM_ checkAssertions base16TestData

case_base16TestsFail = do
    Nothing @=? decode base16 "A"
    Nothing @=? decode base16 "GH"

-- {{{1 yEncoding
case_yencTests = do
    [] @=? Yenc.encode []
    Just [] @=? Yenc.decode []
    [0x90] @=? Yenc.encode [0x66]
    Just [0x66] @=? Yenc.decode [0x90]
    [0x90, 0x99, 0x99, 0x8c, 0x8b, 0x9c] @=? Yenc.encode [0x66, 0x6f, 0x6f, 0x62, 0x61, 0x72]
    Just [0x66, 0x6f, 0x6f, 0x62, 0x61, 0x72] @=? Yenc.decode [0x90, 0x99, 0x99, 0x8c, 0x8b, 0x9c]
    [0x3d, 0x40, 0x01] @=? Yenc.encode [0xd6, 0xd7]
    Just [0xd6, 0xd7] @=? Yenc.decode [0x3d, 0x40, 0x01]
    [0x3d, 0x40, 0x3d, 0x4a, 0x3d, 0x4d, 0x3d, 0x7d] @=? Yenc.encode [0xd6, 0xe0, 0xe3, 0x13]
    Just [0xd6, 0xe0, 0xe3, 0x13] @=? Yenc.decode [0x3d, 0x40, 0x3d, 0x4a, 0x3d, 0x4d, 0x3d, 0x7d]
    [[0x3d, 0x40], [0x01, 0x3d, 0x4a]] @=? Yenc.chop 2 [0x3d, 0x40, 0x01, 0x3d, 0x4a]

-- {{{1 quoted-printable
qpTestData =
    [ ("qp", "empty", "", [], qp)
    , ("qp", "foo=bar", "foo=3Dbar", [102,111,111,61,98,97,114], qp)
    ]
case_qpTests = mapM_ checkAssertions qpTestData

case_qpTestsSucc = do
    ["foo=","=3D=","bar"] @=? chop qp 4 "foo=3Dbar"

case_qpTestsFail = do
    Nothing @=? decode qp "=4"
    Nothing @=? decode qp "=G"

-- {{{1 python string
pyTestData =
    [ ("py", "empty", "", [], py)
    , ("py", "<0x00><0x1f><0x20><0x7e><0x7f><0xff>", "\\x00\\x1F ~\\x7F\\xFF", [0x00, 0x1f, 0x20, 0x7e, 0x7f, 0xff], py)
    , ("py", "\"\'\\", "\\\"\\'\\\\", [34, 39, 92], py)
    ]
case_pyTests = mapM_ checkAssertions pyTestData

case_pyTestsFail = do
    Nothing @=? decode py "\\z"

-- {{{1 url encoding
urlTestData =
    [ ("url", "empty", "", [], url)
    , ("url", "aA", "aA", [97, 65], url)
    , ("url", "~ ", "~%20", [126, 0x20], url)
    ]
case_urlTests = mapM_ checkAssertions urlTestData

case_urlTestsFail = do
    Nothing @=? decode url "%ga"
    Nothing @=? decode url "%%"

-- {{{1 all the tests
allTests = $(testGroupGenerator)
