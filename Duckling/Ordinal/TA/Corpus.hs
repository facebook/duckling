-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.TA.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale TA Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 1)
             [ "முதல்"
             , "1."
             ]
  , examples (OrdinalData 2)
             [ "இரண்டாம்"
             , "2."
             ]
  , examples (OrdinalData 3)
             [ "மூன்றாம்"
             , "3."
             ]
  , examples (OrdinalData 4)
             [ "நான்காம்"
             , "4."
             ]
  , examples (OrdinalData 5)
             [ "ஐந்தாம்"
             , "5."
             ]
  , examples (OrdinalData 6)
             [ "ஆறாம்"
             , "6."
             ]
  , examples (OrdinalData 7)
             [ "ஏழாம்"
             , "7."
             ]
  , examples (OrdinalData 8)
             [ "எட்டாம்"
             , "8."
             ]
  , examples (OrdinalData 9)
             [ "ஒன்பதாம்"
             , "9."
             ]
  , examples (OrdinalData 10)
             [ "பத்தாம்"
             , "10."
             ]
  , examples (OrdinalData 11)
             [ "பதினொன்றாம்"
             , "11."
             ]
  , examples (OrdinalData 12)
             [ "பன்னிரண்டாம்"
             , "12."
             ]
  , examples (OrdinalData 20)
             [ "இருபதாம்"
             , "20."
             ]
  , examples (OrdinalData 21)
             [ "இருபத்திஒன்றாம்"
             , "21."
             ]
  , examples (OrdinalData 22)
             [ "இருபத்திஇரண்டாம்"
             , "22."
             ]
  , examples (OrdinalData 26)
             [ "இருபத்திஆறாம்"
             , "26."
             ]
  , examples (OrdinalData 30)
             [ "முப்பதாம்"
             , "30."
             ]
   , examples (OrdinalData 33)
             [ "முப்பத்துமூன்றாம்"
             , "33."
             ]
  , examples (OrdinalData 50)
             [ "ஐம்பதாம்"
             , "50."
             ]
   , examples (OrdinalData 54)
              [ "ஐம்பத்திநான்காம்"
              , "54."
              ]
   , examples (OrdinalData 65)
              [ "அறுபத்ஐந்தாம்"
              , "65."
              ]
   , examples (OrdinalData 76)
              [ "எழுபத்திஆறாம்"
              , "76."
              ]
   , examples (OrdinalData 87)
              [ "எண்பத்திஏழாம்"
              , "87."
              ]
  ]
