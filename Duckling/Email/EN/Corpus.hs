-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Email.EN.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Email.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext, allExamples)

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
  ]
