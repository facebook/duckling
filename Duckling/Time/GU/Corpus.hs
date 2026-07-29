{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.GU.Corpus
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
corpus = (testContext {locale = makeLocale GU Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "આજે"
             , "આજ"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "આવતીકાલે"
             , "કાલે"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "પરમદિવસે"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "શુક્રવાર"
             , "આવતા શુક્રવારે"
             , "આવતા શુક્રવાર"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "આવતા સોમવારે"
             ]
  , examples (datetime (2013, 2, 15, 15, 45, 0) Minute)
             [ "આવતા શુક્રવારે સાંજે 3:45 વાગ્યે"
             , "આવતા શુક્રવારે બપોરે ૩:૪૫ વાગ્યે"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 15, 45, 0), (2013, 2, 15, 17, 16, 0)) Minute)
             [ "આવતા શુક્રવારે સાંજે 3:45 વાગ્યે 90 મિનિટની"
             ]
  , examples (datetime (2013, 3, 2, 0, 0, 0) Day)
             [ "આવતા મહિનાના પહેલા સોમવારથી બે દિવસ પહેલાં"
             ]
  , examples (datetime (2013, 2, 12, 17, 0, 0) Hour)
             [ "આજે સાંજે 5 વાગ્યે"
             ]
  , examples (datetime (2013, 2, 18, 4, 0, 0) Hour)
             [ "6 દિવસમાં"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 13, 15, 0), (2013, 2, 12, 14, 46, 0)) Minute)
             [ "આજે બપોરે 1:15 વાગ્યે થી 2:45 વાગ્યે વચ્ચે"
             ]
  , examples (datetime (2026, 9, 18, 0, 0, 0) Day)
             [ "18 સપ્ટેમ્બર, 2026"
             , "18 સપ્ટેમ્બર 2026"
             , "૧૮ સપ્ટેમ્બર ૨૦૨૬"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "આ અઠવાડિયે"
             ]
  , examples (datetime (2013, 2, 12, 7, 15, 0) Minute)
             [ "સવા સાત વાગ્યે"
             , "સવા સાત"
             ]
  , examples (datetime (2013, 2, 12, 7, 45, 0) Minute)
             [ "પોણા આઠ વાગ્યે"
             ]
  , examples (datetime (2013, 2, 12, 13, 30, 0) Minute)
             [ "દોઢ વાગ્યે"
             ]
  , examples (datetime (2013, 2, 12, 14, 30, 0) Minute)
             [ "અઢી વાગ્યે"
             ]
  , examples (datetime (2013, 2, 12, 7, 30, 0) Minute)
             [ "સાડા સાત વાગ્યે"
             ]
  , examples (datetime (2013, 3, 12, 10, 30, 0) Minute)
             [ "દર મહિનાના બીજા મંગળવારે સવારે 10:30 વાગ્યે"
             , "દર મહિને બીજા મંગળવારે સવારે 10:30 વાગ્યે"
             ]
  ]
