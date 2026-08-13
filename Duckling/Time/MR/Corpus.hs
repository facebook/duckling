{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.MR.Corpus
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
corpus = (testContext {locale = makeLocale MR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "आज"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "उद्या"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "परवा"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "शुक्रवार"
             , "येत्या शुक्रवारी"
             , "पुढील शुक्रवारी"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "येत्या सोमवारी"
             ]
  , examples (datetime (2013, 2, 15, 15, 45, 0) Minute)
             [ "येत्या शुक्रवारी संध्याकाळी 3:45 वाजता"
             ]
  , examples (datetime (2013, 2, 15, 3, 45, 0) Minute)
             [ "संपूर्ण शुक्रवारी पूर्ण 3:45 मिनिटे"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 15, 45, 0), (2013, 2, 15, 17, 16, 0)) Minute)
             [ "येत्या शुक्रवारी संध्याकाळी 3:45 वाजता 90 मिनिटांची"
             , "पुढील शुक्रवारी दुपारी ३:४५ वाजता जॉनसोबत ९० मिनिटांची"
             ]
  , examples (datetime (2013, 3, 2, 0, 0, 0) Day)
             [ "पुढील महिन्याच्या पहिल्या सोमवारापासून दोन दिवस पूर्वी"
             , "पुढच्या महिन्यातील पहिल्या सोमवारच्या दोन दिवस आधी"
             ]
  , examples (datetime (2013, 2, 12, 17, 0, 0) Hour)
             [ "आज संध्याकाळी 5 वाजता"
             ]
  , examples (datetime (2013, 2, 18, 4, 0, 0) Hour)
             [ "6 दिवसांत"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 13, 15, 0), (2013, 2, 12, 14, 46, 0)) Minute)
             [ "आज दुपारी 1:15 वाजता ते 2:45 वाजता दरम्यान"
             ]
  , examples (datetime (2026, 9, 18, 0, 0, 0) Day)
             [ "18 सप्टेंबर, 2026"
             , "18 सप्टेंबर 2026"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "या आठवड्यात"
             ]
  , examples (datetime (2013, 2, 12, 7, 15, 0) Minute)
             [ "सव्वा सात वाजता"
             , "सव्वा सात"
             ]
  , examples (datetime (2013, 2, 12, 7, 45, 0) Minute)
             [ "पावणे आठ वाजता"
             ]
  , examples (datetime (2013, 2, 12, 13, 30, 0) Minute)
             [ "दीड वाजता"
             ]
  , examples (datetime (2013, 2, 12, 14, 30, 0) Minute)
             [ "अडीच वाजता"
             ]
  , examples (datetime (2013, 2, 12, 7, 30, 0) Minute)
             [ "साडे सात वाजता"
             ]
  , examples (datetime (2013, 2, 13, 6, 15, 0) Minute)
             [ "प्रत्येक दिवशी सकाळी ६:१५"
             ]
  , examples (datetime (2013, 2, 13, 20, 0, 0) Hour)
             [ "उद्या संध्याकाळी साधारण ८ वाजता"
             ]
  , examples (datetime (2013, 2, 13, 7, 15, 0) Minute)
             [ "उद्या सकाळी सव्वासात वाजता"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 30) Second)
             [ "एक तास आणि तीस सेकंदांनी"
             ]
  , examples (datetime (2013, 12, 31, 0, 0, 0) Hour)
             [ "31 डिसेंबरच्या मध्यरात्री"
             ]
  , examples (datetime (2014, 1, 1, 6, 0, 0) Hour)
             [ "1 जानेवारी रोजी सकाळी 6 वाजता"
             ]
  , examples (datetime (2013, 3, 12, 10, 30, 0) Minute)
             [ "दर महिन्याच्या दुसऱ्या मंगळवारी सकाळी 10:30 वाजता"
             ]
  ]
