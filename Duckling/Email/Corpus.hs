-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Email.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Data.String
import Prelude

import Duckling.Email.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext, testOptions, examples)
  where
    examples =
      [ "hey@6"
      , "hey@you"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (EmailData "alice@exAmple.io")
             [ "alice@exAmple.io"
             ]
  , examples (EmailData "yo+yo@blah.org")
             [ "yo+yo@blah.org"
             ]
  , examples (EmailData "1234+abc@x.net")
             [ "1234+abc@x.net"
             ]
  , examples (EmailData "jean-jacques@stuff.co.uk")
             [ "jean-jacques@stuff.co.uk"
             ]
  ]
