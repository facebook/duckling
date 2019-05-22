-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Email.IT.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Email.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale IT Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (EmailData "alice@exAmple.io")
             [ "alice chiocciola exAmple.io"
             ]
  , examples (EmailData "yo+yo@blah.org")
             [ "yo+yo chiocciola blah.org"
             ]
  , examples (EmailData "1234+abc@x.net")
             [ "1234+abc chiocciola x.net"
             ]
  , examples (EmailData "jean-jacques@stuff.co.uk")
             [ "jean-jacques chiocciola stuff.co.uk"
             ]
  ]
