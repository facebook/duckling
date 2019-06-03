-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.HU.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types hiding (add)
import Duckling.Testing.Types hiding (examples)

corpus :: Corpus
corpus = (testContext {locale = makeLocale HU Nothing}, testOptions, allExamples)

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
  , examples (datetime (2013, 3, 15, 0, 0, 0) Day)
             [ "március 15"
             , "már 15"
             , "már. 15"
             , "márc 15"
             , "márc. 15"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "következő hónap"
             , "jövő hónap"
             ]
  , examples (datetime (2012, 1, 1, 0, 0, 0) Year)
             [ "előző év"
             , "múlt év"
             ]
  , examples (datetime (2013, 2, 21, 0, 0, 0) Day)
             [ "jövő csütörtök"
             ]
  , examples (datetime (2013, 2, 12, 15, 20, 0) Minute)
             [ "15:20"
             ]
  , examples (datetime (2013, 2, 12, 8, 20, 0) Minute)
             [ "08:20"
             , "8:20"
             ]
  , examples (datetime (2013, 2, 12, 15, 20, 44) Second)
             [ "15:20:44"
             ]
  , examples (datetime (2013, 2, 12, 8, 20, 44) Second)
             [ "08:20:44"
             , "8:20:44"
             ]
  , examples (datetime (2013, 2, 12, 11, 0, 0) Hour)
             [ "de 11"
             , "de. 11"
             , "délelőtt 11"
             ]
  , examples (datetime (2013, 2, 12, 23, 0, 0) Hour)
             [ "du 11"
             , "du. 11"
             , "délután 11"
             ]
  , examples (datetime (2013, 2, 13, 23, 0, 0) Hour)
             [ "szerda du 11"
             , "szerda du. 11"
             , "szerda délután 11"
             ]
  , examples (datetime (2013, 2, 20, 23, 0, 0) Hour)
             [ "jövő szerda du 11"
             , "jövő szerda du. 11"
             , "jövő szerda délután 11"
             ]
  , examples (datetime (2013, 8, 20, 0, 0, 0) Day)
             [ "2013.08.20"
             , "2013 . 08 . 20"
             , "2013-08-20"
             , "2013 - 08 - 20"
             ]
  , examples (datetime (2013, 8, 20, 11, 45, 0) Minute)
             [ "2013.08.20 11:45"
             ]
  , examples (datetime (2013, 8, 20, 17, 0, 0) Hour)
             [ "2013.08.20 délután 5"
             ]
  , examples (datetime (2013, 8, 20, 0, 0, 0) Day)
             [ "08.20"
             , "08 . 20"
             , "08-20"
             , "08 - 20"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 6, 0, 0), (2013, 2, 12, 10, 0, 0)) Hour)
             [ "ma reggel"
             , "reggel"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 8, 0, 0), (2013, 2, 13, 12, 0, 0)) Hour)
             [ "holnap délelőtt"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 12, 0, 0), (2013, 2, 11, 13, 0, 0)) Hour)
             [ "tegnap délben"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 18, 0, 0)) Hour)
             [ "ma délután"
             , "délután"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 16, 0, 0), (2013, 2, 12, 20, 0, 0)) Hour)
             [ "ma este"
             , "este"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 20, 0, 0), (2013, 2, 12, 23, 0, 0)) Hour)
             [ "ma éjszaka"
             , "éjszaka"
             ]
  , examples (datetimeInterval ((2013, 6, 21, 0, 0, 0), (2013, 9, 24, 0, 0, 0)) Day)
             [ "nyár"
             ]
  ]
