-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Url.Tests
  ( tests
  ) where

import Data.String
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Duckling.Dimensions.Types
import Duckling.Testing.Asserts
import Duckling.Testing.Types
import Duckling.Url.Corpus
import Duckling.Url.Types

tests :: TestTree
tests = testGroup "Url Tests"
  [ makeCorpusTest [This Url] corpus
  , makeNegativeCorpusTest [This Url] negativeCorpus
  , surroundTests
  ]

surroundTests :: TestTree
surroundTests = testCase "Surround Tests" $
  mapM_ (analyzedFirstTest testContext testOptions . withTargets [This Url]) xs
  where
    xs = examples (UrlData "www.lets-try-this-one.co.uk/episode-7" "lets-try-this-one.co.uk")
                  [ "phishing link: www.lets-try-this-one.co.uk/episode-7  If you want my job"
                  ]
