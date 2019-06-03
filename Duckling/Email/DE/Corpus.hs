-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Email.DE.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Data.String
import Prelude
import qualified Data.Text as Text

import Duckling.Email.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

context :: Context
context = testContext {locale = makeLocale DE Nothing}

negativeCorpus :: NegativeCorpus
negativeCorpus = (context, testOptions, examples)
  where
    examples =
      [ "fitness at 6.40"
      , "class at 12.00"
      , "tonight at 9.15"
      , " dot 2@abci"
      , "x@ dot x"
      , "x@ x dot "
      , "abc@x dot "
      , Text.replicate 10000 "a at a"
      ]

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (EmailData "alice@exAmple.io")
             [ "alice at exAmple.io"
             ]
  , examples (EmailData "yo+yo@blah.org")
             [ "yo+yo at blah.org"
             ]
  , examples (EmailData "1234+abc@x.net")
             [ "1234+abc at x.net"
             ]
  , examples (EmailData "jean-jacques@stuff.co.uk")
             [ "jean-jacques at stuff.co.uk"
             ]
  , examples (EmailData "asdf+ab.c@gmail.com")
             [ "asdf+ab punkt c at gmail punkt com"
             ]
  , examples (EmailData "asdf.k@fb.com")
             [ "asdf punkt k@fb punkt com"
             ]
  ]
