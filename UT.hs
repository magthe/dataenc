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

import qualified Codec.Binary.Uu as Uu
import qualified Codec.Binary.Base64 as Base64
import qualified Codec.Binary.Base64Url as Base64Url
import qualified Codec.Binary.Base32 as Base32
import qualified Codec.Binary.Base32Hex as Base32Hex
import qualified Codec.Binary.Base16 as Base16

-- {{{1 buildTestList
-- builds a list of successful tests based on a tuple (suite, description,
-- encoded data, decoded data, encoding function, decoding function)
buildTestList td = TestList $ concat
    [ [TestLabel (suite ++ " encode " ++ desc) (enc ~=? (fenc dec)), TestLabel (suite ++ " decode " ++ desc) (dec ~=? (fromJust $ fdec enc))] 
        | (suite, desc, enc, dec, fenc, fdec) <- td ]

-- {{{1 uuencode tests
uuTestData =
    [ ("uu", "empty", "", [], Uu.encode, Uu.decode)
    , ("uu", "\0", "``", [0], Uu.encode, Uu.decode)
    , ("uu", "\255", "_P", [255], Uu.encode, Uu.decode)
    , ("uu", "Example", "17AA;7!L90", [69,120,97,109,112,108,101], Uu.encode, Uu.decode)
    ]
uuTests = buildTestList uuTestData

uuTestData2 =
    [("uu test5", "chop . encode Example", ["'17AA;7!L90"], [69,120,97,109,112,108,101], (Uu.chop 61 . Uu.encode), (Uu.decode . Uu.unchop))
    ]
uuTests2 = buildTestList uuTestData2

uuTestsFail = test
    [ "uu decode short" ~: Nothing ~=? Uu.decode "A"
    , "uu decode' short" ~: [Nothing] ~=? Uu.decode' "A"
    , "uu decode illegal" ~: Nothing ~=? Uu.decode "aa"
    , "uu decode' illegal" ~: [Nothing] ~=? Uu.decode' "aa"
    ]

-- {{{1 base64 tests
base64TestData =
    [ ("base64", "empty", "", [], Base64.encode, Base64.decode)
    , ("base64", "f", "Zg==", [102], Base64.encode, Base64.decode)
    , ("base64", "fo", "Zm8=", [102,111], Base64.encode, Base64.decode)
    , ("base64", "foo", "Zm9v", [102,111,111], Base64.encode, Base64.decode)
    , ("base64", "foob", "Zm9vYg==", [102,111,111,98], Base64.encode, Base64.decode)
    , ("base64", "fooba", "Zm9vYmE=", [102,111,111,98,97], Base64.encode, Base64.decode)
    , ("base64", "foobar", "Zm9vYmFy", [102,111,111,98,97,114], Base64.encode, Base64.decode)
    , ("base64url", "\0", "AA==", [0], Base64.encode, Base64.decode)
    , ("base64url", "\255", "/w==", [255], Base64.encode, Base64.decode)
    , ("base64", "Example", "RXhhbXBsZQ==", [69,120,97,109,112,108,101], Base64.encode, Base64.decode)
    ]
base64Tests = buildTestList base64TestData

base64TestsFail = test
    [ "base64 decode short" ~: Nothing ~=? Base64.decode "A"
    , "base64 decode' short" ~: [Nothing] ~=? Base64.decode' "A"
    , "base64 decode illegal" ~: Nothing ~=? Base64.decode "!!"
    , "base64 decode' illegal" ~: [Nothing] ~=? Base64.decode' "!!"
    ]

-- {{{1 base64url tests
base64UrlTestData =
    [ ("base64url", "empty", "", [], Base64Url.encode, Base64Url.decode)
    , ("base64url", "\0", "AA==", [0], Base64Url.encode, Base64Url.decode)
    , ("base64url", "\255", "_w==", [255], Base64Url.encode, Base64Url.decode)
    , ("base64url", "Example", "RXhhbXBsZQ==", [69,120,97,109,112,108,101], Base64Url.encode, Base64Url.decode)
    ]
base64UrlTests = buildTestList base64UrlTestData

-- {{{1 base32 tests
base32TestData =
    [ ("base32", "empty", "", [], Base32.encode, Base32.decode)
    , ("base32", "f", "MY======", [102], Base32.encode, Base32.decode)
    , ("base32", "fo", "MZXQ====", [102,111], Base32.encode, Base32.decode)
    , ("base32", "foo", "MZXW6===", [102,111,111], Base32.encode, Base32.decode)
    , ("base32", "foob", "MZXW6YQ=", [102,111,111,98], Base32.encode, Base32.decode)
    , ("base32", "fooba", "MZXW6YTB", [102,111,111,98,97], Base32.encode, Base32.decode)
    , ("base32", "foobar", "MZXW6YTBOI======", [102,111,111,98,97,114], Base32.encode, Base32.decode)
    ]
base32Tests = buildTestList base32TestData

base32TestsFail = test
    [ "base32 decode short" ~: Nothing ~=? Base32.decode "A"
    , "base32 decode' short" ~: [Nothing] ~=? Base32.decode' "A"
    , "base32 decode illegal" ~: Nothing ~=? Base32.decode "gh"
    , "base32 decode' illegal" ~: [Nothing] ~=? Base32.decode' "gh"
    ]

-- {{{1 base32hex tests
base32HexTestData =
    [ ("base32hex", "empty", "", [], Base32Hex.encode, Base32Hex.decode)
    , ("base32hex", "f", "CO======", [102], Base32Hex.encode, Base32Hex.decode)
    , ("base32hex", "fo", "CPNG====", [102,111], Base32Hex.encode, Base32Hex.decode)
    , ("base32hex", "foo", "CPNMU===", [102,111,111], Base32Hex.encode, Base32Hex.decode)
    , ("base32hex", "foob", "CPNMUOG=", [102,111,111,98], Base32Hex.encode, Base32Hex.decode)
    , ("base32hex", "fooba", "CPNMUOJ1", [102,111,111,98,97], Base32Hex.encode, Base32Hex.decode)
    , ("base32hex", "foobar", "CPNMUOJ1E8======", [102,111,111,98,97,114], Base32Hex.encode, Base32Hex.decode)
    ]
base32HexTests = buildTestList base32HexTestData

base32HexTestsFail = test
    [ "base32hex decode short" ~: Nothing ~=? Base32Hex.decode "A"
    , "base32hex decode' short" ~: [Nothing] ~=? Base32Hex.decode' "A"
    , "base32hex decode illegal" ~: Nothing ~=? Base32Hex.decode "gh"
    , "base32hex decode' illegal" ~: [Nothing] ~=? Base32Hex.decode' "gh"
    ]

-- {{{1 base16 (hex)
base16TestData =
    [ ("base16", "empty", "", [], Base16.encode, Base16.decode)
    , ("base16", "f", "66", [102], Base16.encode, Base16.decode)
    , ("base16", "fo", "666F", [102,111], Base16.encode, Base16.decode)
    , ("base16", "foo", "666F6F", [102,111,111], Base16.encode, Base16.decode)
    , ("base16", "foob", "666F6F62", [102,111,111,98], Base16.encode, Base16.decode)
    , ("base16", "fooba", "666F6F6261", [102,111,111,98,97], Base16.encode, Base16.decode)
    , ("base16", "foobar", "666F6F626172", [102,111,111,98,97,114], Base16.encode, Base16.decode)
    ]
base16Tests = buildTestList base16TestData

base16TestsFail = test
    [ "base16 decode short" ~: Nothing ~=? Base16.decode "A"
    , "base16 decode' short" ~: [Nothing] ~=? Base16.decode' "A"
    , "base16 decode illegal" ~: Nothing ~=? Base16.decode "GH"
    , "base16 decode' illegal" ~: [Nothing] ~=? Base16.decode' "GH"
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
