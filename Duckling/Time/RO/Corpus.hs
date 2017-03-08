-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.RO.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Resolve
import Duckling.Time.Corpus
import Duckling.TimeGrain.Types hiding (add)
import Duckling.Testing.Types hiding (examples)

corpus :: Corpus
corpus = (testContext {lang = RO}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "acum"
             , "chiar acum"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "azi"
             , "astazi"
             , "astăzi"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "ieri"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "maine"
             , "mâine"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "luni"
             , "lunea asta"
             , "lunea aceasta"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "Luni, 18 Feb"
             , "Luni, 18 Februarie"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "marti"
             , "marți"
             , "Marti 19"
             , "Marti pe 19"
             , "Marți 19"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "joi"
             , "jo"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "vineri"
             , "vi"
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "sambata"
             , "sâmbătă"
             , "sam"
             , "sa"
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "duminica"
             , "duminică"
             , "du"
             , "dum"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "1 martie"
             , "intai martie"
             ]
  , examples (datetimeInterval ((2013, 6, 19, 0, 0, 0), (2013, 6, 21, 0, 0, 0)) Day)
             [ "iunie 19-20"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "ziua de craciun"
             , "crăciun"
             ]
  ]
