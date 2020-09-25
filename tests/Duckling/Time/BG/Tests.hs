-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.BG.Tests
  ( tests
  ) where

import Data.Aeson
import Data.Aeson.Types ((.:), parseMaybe, withObject)
import Data.String
import Data.Text (Text)
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Asserts
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.BG.Corpus
import Duckling.TimeGrain.Types (Grain(..))
import Duckling.Types (Range(..))

tests :: TestTree
tests = testGroup "BG Tests"
  [ makeCorpusTest [Seal Time] corpus
  , makeNegativeCorpusTest [Seal Time] negativeCorpus
  ]
