-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


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
  [ makeCorpusTest [Some Url] corpus
  , makeNegativeCorpusTest [Some Url] negativeCorpus
  , surroundTests
  ]

surroundTests :: TestTree
surroundTests = testCase "Surround Tests" $
  mapM_ (analyzedFirstTest testContext . withTargets [Some Url]) xs
  where
    xs = examples (UrlValue "www.fuck-comment-spammers-they-just-wont-quit.com/episode-7")
                  [ "phishing link: www.fuck-comment-spammers-they-just-wont-quit.com/episode-7  If you want my job"
                  ]
