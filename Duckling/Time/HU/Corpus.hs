-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.HU.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Lang
import Duckling.Resolve
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types hiding (add)
import Duckling.Testing.Types hiding (examples)

corpus :: Corpus
corpus = (testContext {lang = HU}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "most"
             , "épp most"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "ma"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "holnap"
             ] 
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "holnapután"
             ] 
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "tegnap"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "tegnapelőtt"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "hónap vége"
             , "a hónap vége"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Year)
             [ "év vége"
             , "az év vége"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "hétfő"
             , "hét"
             , "hét."
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "kedd"
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "vasárnap"
             , "vas"
             , "vas."
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Month)
             [ "január"
             , "jan"
             , "jan."
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "március"
             , "már"
             , "már."
             , "márc"
             , "márc"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "következő hónap"
             , "jövő hónap"
             ]
  , examples (datetime (2012, 1, 1, 0, 0, 0) Year)
             [ "előző év"
             , "múlt év"
             ]
  ]