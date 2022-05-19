-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.CA.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale CA Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 1)
             [ "primer"
             , "primera"
             , "primers"
             , "primeres"
             ]
   , examples (OrdinalData 2)
              [ "segon"
              , "segona"
              , "segons"
              , "segones"
              ]
   , examples (OrdinalData 3)
              [ "tercer"
              , "tercera"
              , "tercers"
              , "terceres"
              ]
   , examples (OrdinalData 4)
              [ "quart"
              , "quarta"
              , "quarts"
              , "quartes"
              ]
   , examples (OrdinalData 5)
              [ "cinquè"
              , "cinquena"
              , "cinquens"
              , "cinquenes"
              ]
   , examples (OrdinalData 6)
              [ "sisè"
              , "sisena"
              , "sisens"
              , "sisenes"
              ]
   , examples (OrdinalData 7)
              [ "setè"
              , "setena"
              , "setens"
              , "setenes"
              ]
   , examples (OrdinalData 8)
              [ "vuitè"
              , "vuitena"
              , "vuitens"
              , "vuitenes"
              ]
   , examples (OrdinalData 9)
              [ "novè"
              , "novena"
              , "novens"
              , "novenes"
              ]
   , examples (OrdinalData 10)
              [ "desè"
              , "desena"
              , "desens"
              , "desenes"
              ]
    , examples (OrdinalData 11)
              [ "onzè"
              , "onzena"
              ]
    , examples (OrdinalData 12)
              [ "dotzè"
              , "dotzena"
              ]
    , examples (OrdinalData 13)
              [ "tretzè"
              , "tretzena"
              ]
    , examples (OrdinalData 14)
              [ "catorzè"
              , "catorzena"
              ]
    , examples (OrdinalData 15)
              [ "quinzè"
              , "quinzena"
              ]
    , examples (OrdinalData 16)
              [ "setzè"
              , "setzena"
              ]
    , examples (OrdinalData 17)
              [ "dissetè"
              , "dissetena"
              ]
    , examples (OrdinalData 18)
              [ "divuitè"
              , "divuitena"
              ]
    , examples (OrdinalData 19)
              [ "dinovè"
              , "dinovena"
              ]
    , examples (OrdinalData 20)
              [ "vintè"
              , "vintena"
              ]
  ]
