-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.PT.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Quantity.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale PT Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [  examples (simple Pound 2 (Just "carne"))
             [ "duas libras de carne"
             ]
  , examples (simple Gram 2 Nothing)
             [ "2 gramas"
             , "0,002 kg"
             , "2 g"
             , "2/1000 quilogramas"
             , "2/1000 quilos"
             , "2000 miligramas"
             , "2000 miligramas"
             ]
  , examples (simple Gram 1000 Nothing)
             [ "um quilograma"
             , "um quilo"
             , "um kg"
             , "1 kg"
             , "1000 g"
             ]
  , examples (simple Pound 1 Nothing)
             [ "uma Libra"
             , "1 lb"
             , "uma lb"
             ]
  , examples (simple Cup 3 (Just "acucar"))
             [ "3 copos de acucar"
             , "3 copos de AcucAr"
             ]
  , examples (simple Cup 0.75 Nothing)
             [ "3/4 copo"
             , "0,75 copo"
             ]
  , examples (simple Gram 500 (Just "morangos"))
             [ "500 gramas de morangos"
             , "500g de morangos"
             , "0,5 quilos de morangos"
             , "0,5 kg de morangos"
             , "500000mg de morangos"
             ]
  ]
