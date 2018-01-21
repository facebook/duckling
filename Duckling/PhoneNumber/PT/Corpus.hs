-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.PhoneNumber.PT.Corpus
  ( corpus
  ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.PhoneNumber.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale PT Nothing}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (PhoneNumberValue "6502834757 ext 897")
             [ "(650)-283-4757 ramal 897"
             ]
  ]
