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
             , "sétimas"
             ]
  , examples (OrdinalData 10)
             [ "décimos"
             , "decimos"
             , "décima"
             , "decimas"
             ]
  , examples (OrdinalData 11)
             [ "décimos primeiros"
             , "decimo primeiro"
             , "décimas primeiras"
             , "decima primeira"
             ]
  , examples (OrdinalData 12)
             [ "décimos segundos"
             , "decimo segundo"
             , "décimas segundas"
             , "decima segunda"
             ]
  , examples (OrdinalData 17)
             [ "décimos setimos"
             , "decimo sétimo"
             , "decimas sétimas"
             , "decima setima"
             ]
  , examples (OrdinalData 58)
             [ "quinquagesimas oitavas"
             , "qüinquagesimo oitavo"
             ]
  ]
