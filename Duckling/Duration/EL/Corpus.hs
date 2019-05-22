-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.EL.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Prelude
import Data.String

import Duckling.Duration.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.TimeGrain.Types (Grain(..))

context :: Context
context = testContext {locale = makeLocale EL Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (context, testOptions, examples)
  where
    examples =
      [ "για μήνες"
      , "και ημέρες"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (DurationData 1 Second)
             [ "ένα δευτερόλεπτο"
             , "ενός δευτερολέπτου"
             , "1 δεύτερο"
             , "1\""
             ]
  , examples (DurationData 30 Second)
             [ "30 δευτερόλεπτα"
             , "τριάντα δευτερολέπτων"
             , "μισό λεπτό"
             , "30\""
             ]
  , examples (DurationData 1 Minute)
             [ "ενός λεπτού"
             , "1 λεπτού"
             ]
  , examples (DurationData 2 Minute)
             [ "2 λεπτά"
             , "δίλεπτο"
             , "Δίλεπτο"
             , "δύο λεπτά"
             , "δυο λεπτά"
             , "2'"
             ]
  , examples (DurationData 15 Minute)
             [ "ένα τέταρτο"
             , "δεκαπέντε λεπτά"
             , "δεκαπεντάλεπτο"
             , "Δεκαπεντάλεπτο"
             , "15'"
             ]
  , examples (DurationData 30 Minute)
             [ "μισάωρο"
             , "τριάντα λεπτά"
             , "μισή ώρα"
             , "30'"
             ]
  , examples (DurationData 45 Minute)
             [ "τρία τέταρτα"
             , "σαρανταπεντάλεπτος"
             , "45'"
             ]
  , examples (DurationData 60 Minute)
             [ "60 λεπτά"
             , "εξηντάλεπτο"
             ]
  , examples (DurationData 90 Minute)
             [ "μια και μισή ώρα"
             , "περίπου μια και μισή ώρα"
             , "ακριβώς μια και μισή ώρα"
             , "μιάμιση ώρα"
             , "Μιάμιση ώρα"
             , "1,5 ώρα"
             ]
  , examples (DurationData 5 Hour)
             [ "πεντάωρο"
             , "Πεντάωρο"
             , "5 ώρες"
             ]
  , examples (DurationData 60 Hour)
             [ "δυόμισι μέρες"
             , "60 ώρες"
             , "εξήντα ώρες"
             ]
  , examples (DurationData 15 Day)
             [ "15 μέρες"
             , "δεκαπενθήμερο"
             ]
  , examples (DurationData 30 Day)
             [ "30 μέρες"
             ]
  , examples (DurationData 7 Week)
             [ "εφτά εβδομάδες"
             , "7 βδομάδες"
             ]
  , examples (DurationData 1 Month)
             [ "1 μήνας"
             , "ένα μήνα"
             ]
  , examples (DurationData 3 Quarter)
             [ "3 τρίμηνα"
             ]
  , examples (DurationData 18 Month)
             [ "18 μήνες"
             , "ενάμισης χρόνος"
             , "ένας και μισός χρόνος"
             , "ενάμισι έτος"
             , "ένα και μισό έτος"
             ]
  , examples (DurationData 2 Year)
             [ "δυο χρόνια"
             , "δύο έτη"
             , "διετία"
             , "διετής"
             , "δίχρονο"
             ]
  , examples (DurationData 35 Year)
             [ "τριανταπενταετής"
             , "τριανταπεντάχρονος"
             , "τριανταπενταετία"
             , "35 χρόνια"
             ]
  ]
