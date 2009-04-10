{-
 - Copyright : (c) 2009 Magnus Therning
 - License   : BSD3
 -}

module Main
    where

import Test.Framework

import qualified DataencQC as DQC
import qualified DataencUT as DUT

tests =
    [ testGroup "quickcheck" DQC.allTests
    , testGroup "unit test" DUT.allTests
    ]

main = defaultMain tests
