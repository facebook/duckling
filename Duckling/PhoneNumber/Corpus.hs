-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.PhoneNumber.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Prelude
import Data.String

import Duckling.PhoneNumber.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext, testOptions, examples)
  where
    examples =
      [ "12345"
      , "1234567890123456777777"
      , "12345678901234567"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (PhoneNumberValue "6507018887")
             [ "650-701-8887"
             ]
  , examples (PhoneNumberValue "(+1) 6507018887")
             [ "(+1)650-701-8887"
             , "(+1)   650 - 701  8887"
             , "(+1) 650-701-8887"
             , "+1 6507018887"
             ]
  , examples (PhoneNumberValue "(+33) 146647998")
             [ "+33 1 46647998"
             ]
  , examples (PhoneNumberValue "0620702220")
             [ "06 2070 2220"
             ]
  , examples (PhoneNumberValue "6507018887 ext 897")
             [ "(650)-701-8887 ext 897"
             ]
  , examples (PhoneNumberValue "(+1) 2025550121")
             [ "+1-202-555-0121"
             , "+1 202.555.0121"
             ]
  , examples (PhoneNumberValue "4866827")
             [ "4.8.6.6.8.2.7"
             ]
  , examples (PhoneNumberValue "06354640807")
             [ "06354640807"
             ]
  , examples (PhoneNumberValue "18998078030")
             [ "18998078030"
             ]
  , examples (PhoneNumberValue "61992852776")
             [ "61 - 9 9285-2776"
             ]
  , examples (PhoneNumberValue "19997424919")
             [ "(19) 997424919"
             ]
  , examples (PhoneNumberValue "(+55) 19992842606")
             [ "+55 19992842606"
             ]
  ]
