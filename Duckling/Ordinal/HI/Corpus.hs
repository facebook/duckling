-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.HI.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale HI Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [examples (OrdinalData 0)
            [ "शून्य"
            ]
  , examples (OrdinalData 1)
             [ "प्रथम"
             , "पहला"
             , "पहली"
             , "पहले"
             ]
  , examples (OrdinalData 2)
             [ "द्वितीय"
             , "दूसरा"
             , "दूसरी"
             , "दूसरे"
             ]
  , examples (OrdinalData 3)
             [ "तृतीय"
             , "तीसरा"
             , "तीसरी"
             , "तीसरे"
             ]
  , examples (OrdinalData 4)
             [ "चौथा"
             , "चौथी"
             , "चौथे"
             ]
  , examples (OrdinalData 5)
             [ "पाँचवा"
             , "पाँचवी"
             , "पाँचवे"
             , "5वा"
             , "5वी"
             , "5वे"
             ]
  , examples (OrdinalData 6)
             [ "छठा"
             , "छठी"
             , "छठे"
             ]
  , examples (OrdinalData 10)
             [ "दसवा"
             , "दसवी"
             , "दसवे"
             , "10वा"
             , "10वी"
             , "10वे"
             ]
  , examples (OrdinalData 18)
             [ "अठारहवा"
             , "अठारहवी"
             , "अठारहवे"
             , "18वा"
             , "18वी"
             , "18वे"
              ]
  , examples (OrdinalData 25)
             [ "पच्चीसवा"
             , "पच्चीसवी"
             , "पच्चीसवे"
             , "25वा"
             , "25वी"
             , "25वे"
              ]
  , examples (OrdinalData 50)
             [ "पचासवा"
             , "पचासवी"
             , "पचासवे"
             , "50वा"
             , "50वी"
             , "50वे"
             ]
  , examples (OrdinalData 90)
             [ "नब्बेवा"
             , "नब्बेवी"
             , "नब्बेवे"
             , "90वा"
             , "90वी"
             , "90वे"
             ]
  ]
