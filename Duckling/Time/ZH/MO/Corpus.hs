-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.ZH.MO.Corpus
  ( allExamples
  ) where

import Data.String
import Prelude

import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types hiding (add)

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 10, 1, 0, 0, 0) Day)
             [ "国庆"
             , "國慶"
             , "国庆节"
             , "国庆節"
             , "國慶节"
             , "國慶節"
             ]
  , examples (datetimeInterval ((2013, 10, 1, 18, 0, 0), (2013, 10, 2, 0, 0, 0)) Hour)
             [ "国庆节晚上"
             , "國慶節晚上"
             ]
  ]
