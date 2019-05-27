-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.RO.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale RO Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 1)
             [ "primul"
             , "prima"
             ]
  , examples (OrdinalData 2)
             [ "al doilea"
             , "al doi-lea"
             , "al doi lea"
             , "al 2-lea"
             , "al 2 lea"
             , "a 2 a"
             , "a 2-a"
             , "a doua"
             ]
  , examples (OrdinalData 3)
             [ "al treilea"
             , "al trei-lea"
             , "a treia"
             , "a 3-lea"
             , "a 3a"
             ]
  , examples (OrdinalData 4)
             [ "a patra"
             , "a patru-lea"
             ]
  , examples (OrdinalData 6)
             [ "al saselea"
             , "al șaselea"
             , "al sase-lea"
             , "al șase-lea"
             , "a sasea"
             , "a șase a"
             ]
  , examples (OrdinalData 9)
             [ "al noualea"
             , "al noua-lea"
             , "al noua lea"
             , "al nouălea"
             , "al nouă lea"
             , "a noua"
             ]
  ]
