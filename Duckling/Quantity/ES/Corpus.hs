-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.ES.Corpus
  ( corpus
  ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Quantity.Types
import Duckling.Resolve
import Duckling.Testing.Types

context :: Context
context = testContext { locale = makeLocale ES Nothing }

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Pound 2 (Just "carne"))
             [ "dos libras de carne"
             ]
  , examples (simple Gram 2 Nothing)
             [ "dos gramos"
             , "0,002 kg"
             , "2/1000 kilogramos"
             , "2000 miligramos"
             ]
  , examples (simple Gram 1000 Nothing)
             [ "un kilogramo"
             , "un kg"
             ]
  , examples (simple Pound 1 Nothing)
             [ "una libra"
             , "1 lb"
             , "una lb"
             ]
  , examples (simple Ounce 2 Nothing)
             [ "2 onzas"
             , "2oz"
             ]
  , examples (simple Cup 3 (Just "azucar"))
             [ "tres tazas de azucar"
             , "3 tazas de AzUcAr"
             ]
  , examples (simple Cup 0.75 Nothing)
             [ "3/4 taza"
             , "0,75 taza"
             , ",75 tazas"
             ]
  , examples (simple Gram 500 (Just "fresas"))
             [ "500 gramos de fresas"
             , "500g de fresas"
             , "0,5 kilogramos de fresas"
             , "0,5 kg de fresas"
             , "500000mg de fresas"
             ]
  , examples (under Pound 6 (Just "carne"))
              [ "menos que seis libras de carne"
              , "no más que 6 lbs de carne"
              , "por debajo de 6,0 libras de carne"
              , "a lo sumo seis libras de carne"
              ]
  , examples (above Cup 2 Nothing)
              [ "excesivo 2 tazas"
              , "como mínimo dos tazas"
              , "mayor de 2 tazas"
              , "más de 2 tazas"
              ]
  , examples (above Ounce 4 (Just "chocolate"))
              [ "excesivo 4 oz de chocolate"
              , "al menos 4,0 oz de chocolate"
              , "mayor de cuatro onzas de chocolate"
              , "más de cuatro onzas de chocolate"
              ]
  ]
