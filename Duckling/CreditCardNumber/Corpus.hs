-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.CreditCardNumber.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Prelude
import Data.String
import qualified Data.Text as T

import Duckling.CreditCardNumber.Types
import Duckling.CreditCardNumber.Helpers
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext, testOptions, examples)
  where
    examples =
      [ T.replicate (minNumberDigits - 1) "0"
      , T.replicate (maxNumberDigits + 1) "0"
      , "invalid"
      , "4111111111111110"
      , "41111111-1111-1111"
      , "371449635398430"
      , "3714496353-98431"
      , "6011111111111110"
      , "60111111-1111-1117"
      , "5555555555554440"
      , "55555555-5555-4444"
      , "30569309025900"
      , "3056930902-5904"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples
      (CreditCardNumberValue "4111111111111111" Visa)
      [ "4111111111111111"
      , "4111-1111-1111-1111"
      ]
  , examples
      (CreditCardNumberValue "371449635398431" Amex)
      [ "371449635398431"
      , "3714-496353-98431"
      ]
  , examples
      (CreditCardNumberValue "6011111111111117" Discover)
      [ "6011111111111117"
      , "6011-1111-1111-1117"
      ]
  , examples
      (CreditCardNumberValue "5555555555554444" Mastercard)
      [ "5555555555554444"
      , "5555-5555-5555-4444"
      ]
  , examples
      (CreditCardNumberValue "30569309025904" DinerClub)
      [ "30569309025904"
      , "3056-930902-5904"
      ]
  , examples
      (CreditCardNumberValue "3530111333300000" Other)
      [ "3530111333300000" ]
  ]
