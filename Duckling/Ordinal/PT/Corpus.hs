-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.PT.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale PT Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 1)
             [ "primeira"
             , "primeiros"
             ]
  , examples (OrdinalData 2)
             [ "Segundo"
             , "segundas"
             ]
  , examples (OrdinalData 7)
             [ "setimo"
             , "sétimo"
             , "sétimas"
             ]
  , examples (OrdinalData 10)
             [ "decimos"
             , "décimos"
             , "decima"
             , "décima"
             , "decimas"
             , "décimas"
             ]
  , examples (OrdinalData 11)
             [ "decimos primeiros"
             , "décimos primeiros"
             , "decimo primeiro"
             , "décimo primeiro"
             , "decimas primeiras"
             , "décimas primeiras"
             , "decima primeira"
             , "décima primeira"
             ]
  , examples (OrdinalData 12)
             [ "decimos segundos"
             , "décimos segundos"
             , "decimo segundo"
             , "décimo segundo"
             , "decimas segundas"
             , "décimas segundas"
             , "decima segunda"
             , "décima segunda"
             ]
  , examples (OrdinalData 17)
             [ "decimos setimos"
             , "décimos setimos"
             , "decimo setimo"
             , "décimo sétimo"
             , "decimas setimas"
             , "décimas sétimas"
             , "decima setima"
             , "décima setima"
             ]
  , examples (OrdinalData 58)
             [ "quinquagésimas oitavas"
             , "qüinquagesimo oitavo"
             , "quinquagésimo oitavo"
             ]
  ]
