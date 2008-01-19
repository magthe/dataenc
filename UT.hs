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

module Main
    where

import Test.HUnit
import Control.Monad
import System.Exit
import Data.Maybe

import Codec.Binary.DataEncoding

-- {{{1 buildTestList
-- builds a list of successful tests based on a tuple (suite, description,
-- encoded data, decoded data, codec)
buildTestList td = TestList $ concat
    [[ TestLabel (suite ++ " encode " ++ desc) (enc ~=? (encode codec dec))
     , TestLabel (suite ++ " decode " ++ desc) (dec ~=? (fromJust $ decode codec $ enc))] 
        | (suite, desc, enc, dec, codec) <- td ]

-- {{{1 uuencode tests
uuTestData =
    [ ("uu", "empty", "", [], uu)
    , ("uu", "\0", "``", [0], uu)
    , ("uu", "\255", "_P", [255], uu)
    , ("uu", "Example", "17AA;7!L90", [69,120,97,109,112,108,101], uu)
    ]
uuTests = buildTestList uuTestData

uuTests2 = test
    [ "chop . encode Example" ~: ["'17AA;7!L90"] ~=? (chop uu 61 . encode uu) [69,120,97,109,112,108,101]
    , "decode . unchop Example" ~: [69,120,97,109,112,108,101] ~=? fromJust ((decode uu . unchop uu) ["'17AA;7!L90"])
    ]

uuTestsFail = test
    [ "uu decode short" ~: Nothing ~=? decode uu "A"
    , "uu decode' short" ~: [Nothing] ~=? decode' uu "A"
    , "uu decode illegal" ~: Nothing ~=? decode uu "aa"
    , "uu decode' illegal" ~: [Nothing] ~=? decode' uu "aa"
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
    , ("base64url", "\0", "AA==", [0], base64)
    , ("base64url", "\255", "/w==", [255], base64)
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

-- {{{1 test list and main
testList = TestList
    [ uuTests, uuTests2, uuTestsFail
    , base64Tests, base64TestsFail
    , base64UrlTests
    , base32Tests, base32TestsFail
    , base32HexTests, base32HexTestsFail
    , base16Tests,  base16TestsFail ]

main :: IO ()
main = do
    counts <- runTestTT testList
    when ((errors counts, failures counts) /= (0, 0)) $ exitWith (ExitFailure 1)
