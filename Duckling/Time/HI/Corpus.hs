{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.HI.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types hiding (add)

corpus :: Corpus
corpus = (testContext {locale = makeLocale HI Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "आज"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "कल"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "परसों"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "शुक्रवार"
             , "अगले शुक्रवार"
             , "आने वाले शुक्रवार को"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "अगले सोमवार"
             ]
  , examples (datetime (2013, 2, 15, 15, 45, 0) Minute)
             [ "अगले शुक्रवार शाम 3:45 बजे"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 15, 45, 0), (2013, 2, 15, 17, 16, 0)) Minute)
             [ "अगले शुक्रवार शाम 3:45 बजे 90 मिनट"
             , "अगले शुक्रवार शाम 3:45 बजे 90 मिनट की"
             ]
  , examples (datetime (2013, 3, 2, 0, 0, 0) Day)
             [ "अगले महीने के पहले सोमवार से दो दिन पहले"
             ]
  , examples (datetime (2013, 2, 12, 17, 0, 0) Hour)
             [ "आज शाम 5 बजे"
             ]
  , examples (datetime (2013, 2, 18, 4, 0, 0) Hour)
             [ "6 दिनों में"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 13, 15, 0), (2013, 2, 12, 14, 46, 0)) Minute)
             [ "आज दोपहर 1:15 बजे से 2:45 बजे के बीच"
             ]
  , examples (datetime (2026, 9, 18, 0, 0, 0) Day)
             [ "18 सितंबर, 2026"
             , "18 सितंबर 2026"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "इस सप्ताह"
             , "इस हफ्ते"
             ]
  , examples (datetime (2013, 2, 12, 7, 15, 0) Minute)
             [ "सवा सात बजे"
             , "सवा सात"
             ]
  , examples (datetime (2013, 2, 12, 7, 45, 0) Minute)
             [ "पौने आठ बजे"
             ]
  , examples (datetime (2013, 2, 12, 13, 30, 0) Minute)
             [ "डेढ़ बजे"
             ]
  , examples (datetime (2013, 2, 12, 14, 30, 0) Minute)
             [ "ढाई बजे"
             ]
  , examples (datetime (2013, 2, 12, 7, 30, 0) Minute)
             [ "साढ़े सात बजे"
             ]
  , examples (datetime (2013, 3, 12, 10, 30, 0) Minute)
             [ "हर महीने के दूसरे मंगलवार को 10:30 बजे"
             , "हर महीने दूसरे मंगलवार को 10:30 बजे"
             ]
  ]
