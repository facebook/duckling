-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.PhoneNumber.Tests
  ( tests
  ) where

import Data.String
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Duckling.Dimensions.Types
import Duckling.PhoneNumber.Corpus
import Duckling.PhoneNumber.Types
import Duckling.Testing.Asserts
import Duckling.Testing.Types
import qualified Duckling.PhoneNumber.AR.Tests as AR
import qualified Duckling.PhoneNumber.PT.Tests as PT

tests :: TestTree
tests = testGroup "PhoneNumber Tests"
  [ makeCorpusTest [This PhoneNumber] corpus
  , makeNegativeCorpusTest [This PhoneNumber] negativeCorpus
  , surroundTests
  , PT.tests
  , AR.tests
  ]

surroundTests :: TestTree
surroundTests = testCase "Surround Tests" $
  mapM_ (analyzedFirstTest testContext testOptions .
    withTargets [This PhoneNumber]) xs
  where
    xs = examples (PhoneNumberValue "06354640807")
                  [ "hey 06354640807"
                  , "06354640807 hey"
                  , "hey 06354640807 hey"
                  ]
         ++ examples (PhoneNumberValue "18998078030")
                     [ "a 18998078030 b"
                     ]
