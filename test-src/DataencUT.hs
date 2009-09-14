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
import Test.Framework.Providers.HUnit

import Codec.Binary.DataEncoding
import qualified Codec.Binary.Yenc as Yenc

-- {{{1 buildTestList
-- builds a list of successful tests based on a tuple (suite, description,
-- encoded data, decoded data, codec)
buildTestList td = TestList $ concat [
    [ TestLabel (suite ++ " encode " ++ desc) (enc ~=? encode codec dec)
    , TestLabel (suite ++ " decode " ++ desc) (dec ~=? fromJust (decode codec enc))
    ] | (suite, desc, enc, dec, codec) <- td ]

-- {{{1 unitTest2TFTest
unitTest2TFTest :: Test -> [TFAPI.Test]
unitTest2TFTest t = let
        unitTest2TFTest' desc (TestCase a) = [testCase desc a]
        unitTest2TFTest' desc (TestLabel s t) = unitTest2TFTest' (desc ++ ":" ++ s) t
        unitTest2TFTest' desc (TestList ts) = concatMap (unitTest2TFTest' desc) ts
    in unitTest2TFTest' "" t


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
uuTests = buildTestList uuTestData

uuTests2 = test
    [ "uu unchop.chop" ~: "EI2" ~=? unchop uu (chop uu 1 "EI2")
    , "uu unchop.chop" ~: "EI3-" ~=? unchop uu (chop uu 1 "EI3-")
    , "uu unchop.chop" ~: "EI3-EE" ~=? unchop uu (chop uu 1 "EI3-EE")
    , "uu full circle" ~: [0..255] ~=? fromJust (decode uu $ unchop uu $ chop uu 1 $ encode uu [0..255])
    , "uu full circle" ~: [0..255] ~=? fromJust (decode uu $ unchop uu $ chop uu 61 $ encode uu [0..255])
    , "uu full circle" ~: [0..255] ~=? fromJust (decode uu $ unchop uu $ chop uu 100 $ encode uu [0..255])
    ]

uuTestsFail = test
    [ "uu decode short" ~: Nothing ~=? decode uu "A"
    , "uu decode' short" ~: [Nothing] ~=? decode' uu "A"
    , "uu decode illegal" ~: Nothing ~=? decode uu "aa"
    , "uu decode' illegal" ~: [Nothing] ~=? decode' uu "aa"
    ]

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
xxTests = buildTestList xxTestData

xxTests2 = test
    [ "xx unchop.chop" ~: "EI2" ~=? unchop xx (chop xx 1 "EI2")
    , "xx unchop.chop" ~: "EI3-" ~=? unchop xx (chop xx 1 "EI3-")
    , "xx unchop.chop" ~: "EI3-EE" ~=? unchop xx (chop xx 1 "EI3-EE")
    , "xx full circle" ~: [0..255] ~=? fromJust (decode xx $ unchop xx $ chop xx 1 $ encode xx [0..255])
    , "xx full circle" ~: [0..255] ~=? fromJust (decode xx $ unchop xx $ chop xx 61 $ encode xx [0..255])
    , "xx full circle" ~: [0..255] ~=? fromJust (decode xx $ unchop xx $ chop xx 100 $ encode xx [0..255])
    ]

xxTestsFail = test
    [ "xx decode short" ~: Nothing ~=? decode xx "A"
    , "xx decode' short" ~: [Nothing] ~=? decode' xx "A"
    , "xx decode illegal" ~: Nothing ~=? decode xx "''"
    , "xx decode' illegal" ~: [Nothing] ~=? decode' xx "''"
    ]

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
    ]
base85Tests = buildTestList base85TestData

base85TestsFail = test
    [ "base85 decode short" ~: Nothing ~=? decode base85 "A"
    , "base85 decode' short" ~: [Nothing] ~=? decode' base85 "A"
    , "base85 decode illegal" ~: Nothing ~=? decode base85 "!z"
    , "base85 decode' illegal" ~: [Nothing] ~=? decode' base85 "!z"
    ]

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
base64Tests = buildTestList base64TestData

base64TestsFail = test
    [ "base64 decode short" ~: Nothing ~=? decode base64 "A"
    , "base64 decode' short" ~: [Nothing] ~=? decode' base64 "A"
    , "base64 decode illegal" ~: Nothing ~=? decode base64 "!!"
    , "base64 decode' illegal" ~: [Nothing] ~=? decode' base64 "!!"
    ]

-- {{{1 base64url tests
base64UrlTestData =
    [ ("base64url", "empty", "", [], base64Url)
    , ("base64url", "\0", "AA==", [0], base64Url)
    , ("base64url", "\255", "_w==", [255], base64Url)
    , ("base64url", "Example", "RXhhbXBsZQ==", [69,120,97,109,112,108,101], base64Url)
    ]
base64UrlTests = buildTestList base64UrlTestData

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
base32Tests = buildTestList base32TestData

base32TestsFail = test
    [ "base32 decode short" ~: Nothing ~=? decode base32 "A"
    , "base32 decode' short" ~: [Nothing] ~=? decode' base32 "A"
    , "base32 decode illegal" ~: Nothing ~=? decode base32 "gh"
    , "base32 decode' illegal" ~: [Nothing] ~=? decode' base32 "gh"
    ]

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
base32HexTests = buildTestList base32HexTestData

base32HexTestsFail = test
    [ "base32hex decode short" ~: Nothing ~=? decode base32Hex "A"
    , "base32hex decode' short" ~: [Nothing] ~=? decode' base32Hex "A"
    , "base32hex decode illegal" ~: Nothing ~=? decode base32Hex "gh"
    , "base32hex decode' illegal" ~: [Nothing] ~=? decode' base32Hex "gh"
    ]

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
base16Tests = buildTestList base16TestData

base16TestsFail = test
    [ "base16 decode short" ~: Nothing ~=? decode base16 "A"
    , "base16 decode' short" ~: [Nothing] ~=? decode' base16 "A"
    , "base16 decode illegal" ~: Nothing ~=? decode base16 "GH"
    , "base16 decode' illegal" ~: [Nothing] ~=? decode' base16 "GH"
    ]

-- {{{1 yEncoding
yencTests = test
    [ "yEnc encode empty" ~: [] ~=? Yenc.encode []
    , "yEnc decode empty" ~: Just [] ~=? Yenc.decode []
    , "yEnc encode 'f'" ~: [0x90] ~=? Yenc.encode [0x66]
    , "yEnc decode enc('f')" ~: Just [0x66] ~=? Yenc.decode [0x90]
    , "yEnc encode 'foobar'" ~: [0x90, 0x99, 0x99, 0x8c, 0x8b, 0x9c] ~=? Yenc.encode [0x66, 0x6f, 0x6f, 0x62, 0x61, 0x72]
    , "yEnc decode enc('foobar')" ~: Just [0x66, 0x6f, 0x6f, 0x62, 0x61, 0x72] ~=? Yenc.decode [0x90, 0x99, 0x99, 0x8c, 0x8b, 0x9c]
    , "yEnc encode [0xd6, 0xd7]" ~: [0x3d, 0x40, 0x01] ~=? Yenc.encode [0xd6, 0xd7]
    , "yEnc decode enc([0xd6, 0xd7])" ~:  Just [0xd6, 0xd7] ~=? Yenc.decode [0x3d, 0x40, 0x01]
    , "yEnc encode criticals" ~: [0x3d, 0x40, 0x3d, 0x4a, 0x3d, 0x4d, 0x3d, 0x7d] ~=? Yenc.encode [0xd6, 0xe0, 0xe3, 0x13]
    , "yEnc decode criticals" ~:  Just [0xd6, 0xe0, 0xe3, 0x13] ~=? Yenc.decode [0x3d, 0x40, 0x3d, 0x4a, 0x3d, 0x4d, 0x3d, 0x7d]
    , "yEnc chop" ~: [[0x3d, 0x40], [0x01, 0x3d, 0x4a]] ~=? Yenc.chop 2 [0x3d, 0x40, 0x01, 0x3d, 0x4a]
    ]

-- {{{1 quoted-printable
qpTestData =
    [ ("qp", "empty", "", [], qp)
    , ("qp", "foo=bar", "foo=3Dbar", [102,111,111,61,98,97,114], qp)
    ]
qpTests = buildTestList qpTestData

qpTestsSucc = test
    [ "qp chop one" ~: ["foo=","=3D=","bar"] ~=? chop qp 4 "foo=3Dbar"
    ]

qpTestsFail = test
    [ "qp decode short" ~: Nothing ~=? decode qp "=4"
    , "qp decode' short" ~: [Nothing] ~=? decode' qp "=4"
    , "qp decode non-hex" ~: Nothing ~=? decode qp "=G"
    , "qp decode' non-hex" ~: [Nothing] ~=? decode' qp "=G"
    ]

-- {{{1 python string
pyTestData =
    [ ("py", "empty", "", [], py)
    , ("py", "<0x00><0x1f><0x20><0x7e><0x7f><0xff>", "\\x00\\x1F ~\\x7F\\xFF", [0x00, 0x1f, 0x20, 0x7e, 0x7f, 0xff], py)
    , ("py", "\"\'\\", "\\\"\\'\\\\", [34, 39, 92], py)
    ]
pyTests = buildTestList pyTestData

pyTestsFail = test
    [ "py decode bad char after \\" ~: Nothing ~=? decode py "\\z"
    , "py decode' bad char after \\" ~: [Nothing] ~=? decode' py "\\z"
    ]

-- {{{1 url encoding
urlTestData =
    [ ("url", "empty", "", [], url)
    , ("url", "aA", "aA", [97, 65], url)
    , ("url", "~ ", "~%20", [126, 0x20], url)
    ]
urlTests = buildTestList urlTestData

urlTestsFail = test
    [ "url decode bad char after %" ~: Nothing ~=? decode url "%ga"
    , "url decode' bad char after %" ~: [Nothing] ~=? decode' url "%ga"
    , "url decode bad char after %" ~: Nothing ~=? decode url "%%"
    , "url decode' bad char after %" ~: [Nothing] ~=? decode' url "%%"
    ]

-- {{{1 all the tests
allTests = concat
    [ unitTest2TFTest uuTests
    , unitTest2TFTest uuTests2
    , unitTest2TFTest uuTestsFail
    , unitTest2TFTest xxTests
    , unitTest2TFTest xxTests2
    , unitTest2TFTest xxTestsFail
    , unitTest2TFTest base85Tests
    , unitTest2TFTest base85TestsFail
    , unitTest2TFTest base64Tests
    , unitTest2TFTest base64TestsFail
    , unitTest2TFTest base64UrlTests
    , unitTest2TFTest base32Tests
    , unitTest2TFTest base32TestsFail
    , unitTest2TFTest base32HexTests
    , unitTest2TFTest base32HexTestsFail
    , unitTest2TFTest base16Tests
    , unitTest2TFTest base16TestsFail
    , unitTest2TFTest yencTests
    , unitTest2TFTest qpTests
    , unitTest2TFTest qpTestsSucc
    , unitTest2TFTest qpTestsFail
    , unitTest2TFTest pyTests
    , unitTest2TFTest pyTestsFail
    , unitTest2TFTest urlTests
    , unitTest2TFTest urlTestsFail
    ]
