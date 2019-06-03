-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Email.IS.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Email.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale IS Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (EmailData "alice@exAmple.io")
             [ "alice ingi exAmple.io"
             ]
  , examples (EmailData "yo+yo@blah.org")
             [ "yo+yo ingi blah.org"
             ]
  , examples (EmailData "1234+abc@x.net")
             [ "1234+abc ingi x.net"
             ]
  , examples (EmailData "jean-jacques@stuff.co.uk")
             [ "jean-jacques ingi stuff.co.uk"
             ]
  ]
