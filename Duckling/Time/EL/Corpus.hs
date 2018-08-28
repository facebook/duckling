-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.EL.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types hiding (add)

context :: Context
context = testContext {locale = makeLocale EL Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (context, testOptions, examples)
  where
    examples =
      [ "ένας ενήληκας"
      , "τρεισήμισι και μισή"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 1, 0, 0, 0) Day)
             [ "1/2/2013"
             ]
  , examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "τώρα"
             , "τώρα αμέσως"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "δευτέρα"
             , "δευτέρας"
             , "την δευτέρα"
             , "αυτή τη δευτέρα"
             , "Δευτέρα, 18 Φεβρουαρίου"
             , "Δευτ, 18 Φλεβάρη"
             , "18 Φεβρουαρίου, Δευτ"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "Τρίτη η 19η Φλεβάρη"
             , "Τρίτη, 19 Φεβρουαρίου"
             , "19 Φεβρουαρίου, Τρίτη"
             , "Τρίτη"
             , "την Τρίτη"
             , "την Τρίτη"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "αύριο"
             , "Τετάρτη"
             , "την Τετάρτη"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "Πέμπτη"
             , "πέμπτη"
             , "Πεμ"
             , "πεμ."
             , "την Πέμπτη"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "παρασκευή"
             , "παρασκευής"
             , "παρ."
             , "την παρασκευή"
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "σάββατο"
             , "σαββάτο"
             , "σαβ."
             , "την σάββάτον"
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "Κυριακή"
             , "κυρ"
             , "κυρ."
             , "την κυριακή"
             ]
  , examples (datetime (2013, 8, 13, 0, 0, 0) Day)
             [ "Τρίτη, 13η Αυγούστου"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Month)
             [ "ιαν"
             , "ιανουάριος"
             , "ιανουαρίου"
             , "γενάρης"
             , "γενάρη"
             ]
  , examples (datetime (2013, 2, 1, 0, 0, 0) Month)
             [ "φεβ"
             , "φλεβάρης"
             , "φλεβάρη"
             , "φεβρουάριο"
             , "φεβρουαρίου"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "μάρτης"
             , "μάρτη"
             , "μαρτίου"
             , "μάρτιο"
             , "μάρτιον"
             , "μαρ"
             ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Month)
             [ "απρίλης"
             , "απρ"
             , "απριλίου"
             , "απριλη"
             , "απρίλιον"
             ]
  , examples (datetime (2013, 5, 1, 0, 0, 0) Month)
             [ "μάη"
             , "μαϊου"
             , "μάιο"
             , "μάης"
             ]
  , examples (datetime (2013, 6, 1, 0, 0, 0) Month)
             [ "ιούνιος"
             , "ιουνίου"
             , "ιούνη"
             , "ιούνιον"
             , "ιουν"
             ]
  , examples (datetime (2013, 7, 1, 0, 0, 0) Month)
             [ "Ιουλ"
             , "ιουλίου"
             , "ιούλη"
             , "ιούλιο"
             ]
  , examples (datetime (2013, 8, 1, 0, 0, 0) Month)
             [ "αυγ"
             , "αυγούστου"
             , "αύγουστο"
             ]
  , examples (datetime (2013, 9, 1, 0, 0, 0) Month)
             [ "σεπτ"
             , "σεπτέμβριος"
             , "σεπτεμβρίου"
             ]
  , examples (datetime (2013, 10, 1, 0, 0, 0) Month)
             [ "Οκτ"
             , "οκτώβρης"
             , "οκτώβρη"
             , "οκτωβρίου"
             , "οκτώβριον"
             ]
  , examples (datetime (2013, 11, 1, 0, 0, 0) Month)
             [ "νοεμ"
             , "νοέμβρης"
             , "νοέμβρη"
             , "νοεμβρίου"
             , "νοέμβριον"
             ]
  , examples (datetime (2013, 12, 1, 0, 0, 0) Month)
             [ "δεκ"
             , "δεκέμβρης"
             , "δεκέμβρη"
             , "δεκεμβρίου"
             , "δεκέμβριον"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "η πρώτη του μάρτη"
             , "πρώτη του μάρτη"
             , "πρώτη μάρτη"
             ]
  , examples (datetime (2013, 4, 3, 0, 0, 0) Day)
             [ "τρεις απρίλη"
             , "τρίτη απρίλη"
             ]
  , examples (datetime (2014, 1, 3, 0, 0, 0) Day)
             [ "τρίτη γενάρη"
             ]
  , examples (datetime (2013, 2, 3, 0, 0, 0) Day)
             [ "τρίτη φλεβάρη"
             ]
  , examples (datetime (2013, 3, 3, 0, 0, 0) Day)
             [ "τρίτη μάρτη"
             ]
  , examples (datetime (2013, 5, 3, 0, 0, 0) Day)
             [ "τρίτη μαϊου"
             ]
  , examples (datetime (2013, 9, 3, 0, 0, 0) Day)
             [ "3 σεπτέμβρη"
             , "3 σεπτεμβρίου"
             , "τρίτη σεπτεμβρίου 2013"
             ]
  , examples (datetime (2013, 9, 3, 0, 0, 0) Day)
             [ "3 σεπτέμβρη"
             , "3 σεπτεμβρίου"
             , "τρίτη σεπτεμβρίου"
             , "τρίτη σεπτεμβρίου 2013"
             ]
  , examples (datetime (2013, 10, 3, 0, 0, 0) Day)
             [ "τρίτη οκτ"
             , "τρίτη οκτώβρη"
             ]
  , examples (datetime (2013, 10, 5, 0, 0, 0) Day)
             [ "πέμπτη οκτωβρίου"
             , "πέμπτη οκτώβρη"
             ]
  , examples (datetime (2013, 9, 3, 0, 0, 0) Day)
             [ "3 σεπτέμβρη"
             , "3 σεπτεμβρίου"
             , "τρίτη σεπτεμβρίου 2013"
             ]
  , examples (datetime (2016, 3, 3, 0, 0, 0) Day)
             [ "τρίτη μάρτη 2016"
             , "3 μαρτίου 2016"
             ]
  , examples (datetime (2016, 4, 5, 0, 0, 0) Day)
             [ "πέμπτη απρίλη 2016"
             , "5 απρ 2016"
             ]
  , examples (datetime (2016, 4, 3, 0, 0, 0) Day)
             [ "τρίτη απρίλη 2016"
             , "3 απρίλης 2016"
             ]
  , examples (datetime (2016, 4, 1, 0, 0, 0) Month)
             [ "απρίλη 2016"
             , "απρίλης 2016"
             ]
  , examples (datetime (2017, 10, 1, 0, 0, 0) Month)
             [ "Οκτ 2017"
             , "Οκτώβρης 2017"
             , "Οκτώβριος 2017"
             ]
  , examples (datetime (2016, 3, 3, 0, 0, 0) Day)
             [ "τρίτη μάρτη 2016"
             , "3η μάρτη 2016"
             , "πέμπτη τρίτη μάρτη 2016"
             ]
  , examples (datetime (2013, 3, 25, 0, 0, 0) Day)
             [ "ευαγγελισμός της θεοτόκου"
             , "ευαγγελισμού της θεοτόκου"
             ]
  , examples (datetime (2015, 3, 3, 0, 0, 0) Day)
             [ "3 μαρτίου 2015"
             , "3η μαρτίου 2015"
             , "τρεις μαρτίου 2015"
             , "3/3/2015"
             , "3/3/15"
             , "2015-3-3"
             , "2015-03-03"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "στις 15 Φεβρουαρίου"
             , "η 15η Φεβρουαρίου"
             , "15 Φεβρουαρίου"
             , "15η Φεβρουαρίου"
             ]
  , examples (datetime (2013, 8, 8, 0, 0, 0) Day)
             [ "8 Αυγ"
             ]
  , examples (datetime (2014, 7, 18, 0, 0, 0) Day)
             [ "Παρασκευή, 18 Ιουλίου"
             , "Ιουλίου 18, Παρασκευή"
             ]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Month)
             [ "Οκτώβριος 2014"
             ]
  , examples (datetime (2015, 4, 14, 0, 0, 0) Day)
             [ "14απριλίου 2015"
             , "14 Απριλίου, 2015"
             , "14η Απριλίου 15"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "την επόμενη Τρίτη"
             , "περίπου την επόμενη Τρίτη"
             ]
  , examples (datetime (2013, 2, 21, 0, 0, 0) Day)
             [ "την επόμενη πέμπτη"
             , "περίπου την επόμενη Πέμπτη"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "μεθεπόμενη παρασκευή"
             ]
  , examples (datetime (2014, 3, 1, 0, 0, 0) Month)
             [ "επόμενος Μάρτης"
             ]
  , examples (datetime (2015, 3, 1, 0, 0, 0) Month)
             [ "μεθεπόμενος Μάρτης"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "Κυριακή, 10 Φεβρουαρίου"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "Τετ, 13Φεβ"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "αυτή τη βδομάδα"
             , "τρέχουσα εβδομάδα"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "περασμένη εβδομάδα"
             , "προηγούμενη εβδομάδα"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "επόμενη εβδομάδα"
             , "περίπου την επόμενη εβδομάδα"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Month)
             [ "τον προηγούμενο μήνα"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "τον επόμενο μήνα"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Quarter)
             [ "αυτό το τρίμηνο"
             ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Quarter)
             [ "το επόμενο τρίμηνο"
             ]
  , examples (datetime (2013, 7, 1, 0, 0, 0) Quarter)
             [ "τρίτο τρίμηνο"
             , "3ο τρίμηνο"
             , "το τρίτο τρίμηνο"
             ]
  , examples (datetime (2018, 10, 1, 0, 0, 0) Quarter)
             [ "4ο τρίμηνο του 2018"
             , "4ο τρίμηνο 2018"
             , "το 4ο τρίμηνο του 2018"
             ]
  , examples (datetime (2012, 1, 1, 0, 0, 0) Year)
             [ "πέρσι"
             , "πέρυσι"
             , "την περασμένη χρονιά"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Year)
             [ "φέτος"
             , "εφέτος"
             , "αυτή τη χρονιά"
             , "αυτό το έτος"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Year)
             [ "τουχρόνου"
             , "του χρόνου"
             , "την επόμενη χρονιά"
             , "τον επόμενο χρόνο"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "την προηγούμενη Κυριακή"
             , "την περασμένη Κυριακή"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "περασμένη τρίτη"
             ]
  , examples (datetime (2013, 2, 7, 0, 0, 0) Day)
             [ "περασμένη πέμπτη"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "επόμενη Τρίτη"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "επόμενη τετάρτη"
             ]
  , examples (datetime (2013, 2, 21, 0, 0, 0) Day)
             [ "επόμενη πέμπτη"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "Τετάρτη της επόμενης εβδομάδας"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "τη μεθεπόμενη παρασκευή"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "χθες"
             , "εχθές"
             , "δευτέρα αυτής της εβδομάδας"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "τρίτη αυτής της εβδομάδας"
             , "σήμερα"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "τετάρτη αυτής της εβδομάδας"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "μεθαύριο"
             ]
  , examples (datetime (2013, 2, 14, 17, 0, 0) Hour)
             [ "μεθαύριο στις 5μμ"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "προχτές"
             ]
  , examples (datetime (2013, 2, 10, 8, 0, 0) Hour)
             [ "προχθές στις 8πμ"
             ]
  , examples (datetime (2013, 3, 25, 0, 0, 0) Day)
             [ "τελευταία Δευτέρα του Μάρτη"
             ]
  , examples (datetime (2014, 3, 30, 0, 0, 0) Day)
             [ "τελευταία Κυριακή του Μάρτη του 2014"
             ]
  , examples (datetime (2013, 10, 3, 0, 0, 0) Day)
             [ "τρίτη μέρα του Οκτώβρη"
             ]
  , examples (datetime (2014, 10, 6, 0, 0, 0) Week)
             [ "πρώτη βδομάδα του Οκτώβρη του 2014"
             ]
  , examples (datetime (2013, 10, 7, 0, 0, 0) Week)
             [ "η εβδομάδα της 6ης Οκτωβρίου"
             ]
  , examples (datetime (2013, 10, 7, 0, 0, 0) Week)
             [ "η εβδομάδα της 7ης Οκτωβρίου"
             ]
  , examples (datetime (2015, 10, 31, 0, 0, 0) Day)
             [ "η τελευταία μέρα του Οκτωβρίου του 2015"
             , "η τελευταία μέρα στον Οκτώβρη του 2015"
             ]
  , examples (datetime (2014, 9, 22, 0, 0, 0) Week)
             [ "η τελευταία βδομάδα Σεπτεμβρίου του 2014"
             ]
  , examples (datetime (2013, 10, 1, 0, 0, 0) Day)
             [ "πρώτη Τρίτη του Οκτώβρη"
             ]
  , examples (datetime (2013, 2, 12, 16, 0, 0) Hour)
             [ "Τρίτη στις 4 το απόγευμα"
             ]
  , examples (datetime (2013, 2, 13, 16, 0, 0) Hour)
             [ "Τετάρτη στις 4 το απόγευμα"
             ]
  , examples (datetime (2014, 9, 16, 0, 0, 0) Day)
             [ "τρίτη Τρίτη του Σεπτ 2014"
             ]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Day)
             [ "πρώτη Τετάρτη του Οκτωβρίου 2014"
             ]
  , examples (datetime (2014, 10, 8, 0, 0, 0) Day)
             [ "δεύτερη Τετάρτη του Οκτωβρίου 2014"
             ]
  , examples (datetime (2015, 1, 8, 0, 0, 0) Day)
             [ "Τρίτη Πέμπτη μετά τα Χριστούγεννα του 2014"
             ]
  , examples (datetime (2013, 2, 13, 3, 0, 0) Hour)
             [ "στις 3πμ"
             , "3 το πρωί"
             , "3πμ"
             , "3 η ώρα το πρωί"
             , "στις 3 πμ"
             ]
  , examples (datetime (2013, 2, 13, 3, 18, 0) Minute)
             [ "3:18πμ"
             , "3:18π"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "στις 3μμ"
             , "@ 3μμ"
             , "3ΜΜ"
             , "3μμ"
             , "3 η ώρα μμ"
             , "3 η ώρα το απόγευμα"
             , "3 το απόγευμα"
             , "3 και κάτι μμ"
             , "3μμ περίπου"
             , "περίπου 3μμ"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ "στα 15 λεπτά μετά τις 3μμ"
             , "ένα τέταρτο μετά τις 3μμ"
             , "3 και τέταρτο μμ"
             , "3:15 το απόγευμα"
             , "15:15"
             , "3:15μμ"
             , "3:15ΜΜ"
             , "3:15μ"
             , "στις 3 και 15"
             ]
  , examples (datetime (2013, 2, 12, 15, 20, 0) Minute)
             [ "20 λεπτά μετά τις 3μμ"
             , "3:20 το απόγευμα"
             , "είκοσι μετά τις 3μμ"
             , "3:20μ"
             , "στις τρεις και είκοσι"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ "στις τρεις και μισή μμ"
             , "τρεισήμισι μμ"
             , "Τρεισήμισι μμ"
             , "15:30"
             , "3:30μμ"
             , "3:30ΜΜ"
             , "330 μ.μ."
             , "3:30 μ μ"
             , "3:30 μμ"
             , "τρεις και μισή"
             ]
  , examples (datetime (2013, 2, 12, 15, 23, 24) Second)
             [ "15:23:24"
             ]
  , examples (datetime (2013, 2, 12, 11, 45, 0) Minute)
             [ "ένα τέταρτο πριν το μεσημέρι"
             , "11:45πμ"
             , "δώδεκα παρά τέταρτο"
             , "12 παρά τέταρτο"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "απόψε στις 8"
             , "8 το βράδυ"
             , "στις 8 το βράδυ"
             ]
  , examples (datetime (2013, 9, 20, 19, 30, 0) Minute)
             [ "στις 7:30 μμ την Παρασκευή, 20 Σεπτ"
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "το Σάββατο στις 9πμ"
             ]
  , examples (datetime (2014, 7, 18, 19, 0, 0) Minute)
             [ "Παρ, 18 Ιουλ, 2014 07:00 μμ"
             ]
  , examples (datetime (2013, 2, 12, 4, 30, 1) Second)
             [ "σε ένα δεύτερο"
             , "σε ένα δευτερόλεπτο από τώρα"
             , "σε 1\""
             ]
  , examples (datetime (2013, 2, 12, 4, 31, 0) Second)
             [ "σε ένα λεπτό"
             , "σε 1'"
             ]
  , examples (datetime (2013, 2, 12, 4, 32, 0) Second)
             [ "σε 2 λεπτά"
             , "σε 2 λεπτά ακόμα"
             , "2 λεπτά από τώρα"
             ]
  , examples (datetime (2013, 2, 12, 11, 40, 0) Minute)
             [ "20 λεπτά πριν τις 12"
             , "11:40"
             ]
  , examples (datetime (2013, 2, 12, 12, 20, 0) Minute)
             [ "20 λεπτά μετά τις 12"
             , "12:20"
             ]
  , examples (datetime (2013, 2, 12, 4, 33, 0) Second)
             [ "σε τρία λεπτά"
             , "σε μερικά λεπτά"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Second)
             [ "σε 60 λεπτά"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "σε ένα τέταρτο της ώρας"
             , "σε 1/4ω"
             , "σε 1/4 ω"
             , "σε 1/4 της ώρας"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "σε μισή ώρα"
             , "σε 1/2ω"
             , "σε 1/2 ω"
             , "σε 1/2 ώρα"
             ]
  , examples (datetime (2013, 2, 12, 5, 15, 0) Second)
             [ "σε τρία τέταρτα της ώρας"
             , "σε 3/4ω"
             , "σε 3/4 ω"
             , "σε 3/4 της ώρας"
             ]
  , examples (datetime (2013, 2, 12, 7, 0, 0) Second)
             [ "σε 2,5 ώρες"
             , "σε 2 και μισή ώρες"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Minute)
             [ "σε μία ώρα"
             , "σε 1ω"
             ]
  , examples (datetime (2013, 2, 12, 6, 30, 0) Minute)
             [ "σε δύο ώρες"
             , "σε δυο ώρες"
             ]
  , examples (datetime (2013, 2, 12, 7, 30, 0) Minute)
             [ "σε μερικές ώρες"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Minute)
             [ "σε 24 ώρες"
             ]
  , examples (datetime (2013, 2, 13, 4, 0, 0) Hour)
             [ "σε μία μέρα"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Second)
             [ "σε μία μέρα από τώρα αμέσως"
             , "σε μία μέρα από τώρα"
             ]
  , examples (datetime (2016, 2, 12, 0, 0, 0) Day)
             [ "3 χρόνια από σήμερα"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "σε 7 μέρες"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "σε μία βδομάδα"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "σε περίπου μισή ώρα"
             ]
  , examples (datetime (2013, 2, 5, 4, 0, 0) Hour)
             [ "πριν από 7 μέρες"
             ]
  , examples (datetime (2013, 1, 29, 4, 0, 0) Hour)
             [ "πριν από 14 μέρες"
             , "14 μέρες πριν"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "πριν από μια βδομάδα"
             , "πριν από 1 βδομάδα"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ "πριν από τρεις βδομάδες"
             ]
  , examples (datetime (2012, 11, 12, 0, 0, 0) Day)
             [ "πριν από τρεις μήνες"
             ]
  , examples (datetime (2011, 2, 1, 0, 0, 0) Month)
             [ "πριν από δύο χρόνια"
             ]
  , examples (datetime (1954, 1, 1, 0, 0, 0) Year)
             [ "1954"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "εδώ και 7 μέρες"
             ]
  , examples (datetime (2013, 2, 26, 4, 0, 0) Hour)
             [ "εδώ και 14 μέρες"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "εδώ και μια βδομάδα"
             , "εδώ και 1 βδομάδα"
             ]
  , examples (datetime (2013, 3, 5, 0, 0, 0) Day)
             [ "εδώ και τρεις βδομάδες"
             ]
  , examples (datetime (2013, 5, 12, 0, 0, 0) Day)
             [ "εδώ και τρεις μήνες"
             ]
  , examples (datetime (2015, 2, 1, 0, 0, 0) Month)
             [ "εδώ και δύο χρόνια"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "ένα χρόνο μετά τα Χριστούγεννα"
             ]
  , examples (datetimeInterval ((2013, 12, 18, 0, 0, 0), (2013, 12, 29, 0, 0, 0)) Day)
             [ "για 10 μέρες μετά τις 18 Δεκεμβρίου"
             ]
  , examples (datetimeInterval ((2013, 9, 23, 0, 0, 0), (2013, 12, 22, 0, 0, 0)) Day)
             [ "αυτό το φθινόπωρο"
             , "αυτού του φθινοπώρου"
             ]
  , examples (datetimeInterval ((2013, 6, 21, 0, 0, 0), (2013, 9, 24, 0, 0, 0)) Day)
             [ "αυτό το καλοκαίρι"
             , "το τρέχον καλοκαίρι"
             ]
  , examples (datetimeInterval ((2012, 12, 21, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "αυτό το χειμώνα"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "Χριστούγεννα"
             ]
  , examples (datetime (2013, 12, 31, 0, 0, 0) Day)
             [ "παραμονή πρωτοχρονιάς"
             , "παραμονές πρωτοχρονιάς"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Day)
             [ "ανήμερα πρωτοχρονιάς"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "του Αγίου Βαλεντίνου"
             , "ημέρα του Αγίου Βαλεντίνου"
             ]
  , examples (datetime (2013, 5, 12, 0, 0, 0) Day)
             [ "ημέρα της μητέρας"
             , "η μέρα της μητέρας"
             ]
  , examples (datetime (2012, 5, 13, 0, 0, 0) Day)
             [ "προηγούμενη μέρα της μητέρας"
             ]
  , examples (datetime (2014, 5, 11, 0, 0, 0) Day)
             [ "η μέρα της μητέρας του 2014"
             ]
  , examples (datetime (2013, 6, 16, 0, 0, 0) Day)
             [ "η μέρα του πατέρα"
             ]
  , examples (datetime (2012, 6, 17, 0, 0, 0) Day)
             [ "προηγούμενη μέρα του πατέρα"
             ]
  , examples (datetime (1996, 6, 16, 0, 0, 0) Day)
             [ "η μέρα του πατέρα το 1996"
             ]
  , examples (datetime (2013, 3, 25, 0, 0, 0) Day)
             [ "η μέρα της επανάστασης"
             ]
  , examples (datetime (2012, 3, 25, 0, 0, 0) Day)
             [ "η περασμένη μέρα της επανάστασης"
             , "η μέρα της επανάστασης της περασμένης χρονιάς"
             , "η μέρα της επανάστασης της περσινής χρονιάς"
             , "η μέρα της επανάστασης πέρσι"
             , "η μέρα της επανάστασης πέρυσι"
             ]
  , examples (datetime (2013, 10, 31, 0, 0, 0) Day)
             [ "halloween"
             , "το halloween του 2013"
             ]
  , examples (datetime (2014, 10, 31, 0, 0, 0) Day)
             [ "το επόμενο halloween"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "απόψε"
             , "σήμερα το βράδυ"
             , "απόψε το βράδυ"
             ]
  , examples (datetimeInterval ((2013, 2, 8, 18, 0, 0), (2013, 2, 11, 0, 0, 0)) Hour)
             [ "το περασμένο σαββατοκύριακο"
             , "το περασμένο ΣΚ"
             , "το προηγούμενο σαββατοκύριακο"
             , "το προηγούμενο σκ"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 18, 0, 0), (2013, 2, 14, 0, 0, 0)) Hour)
             [ "αύριο βράδυ"
             , "αύριο το βράδυ"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 12, 0, 0), (2013, 2, 13, 18, 0, 0)) Hour)
             [ "αύριο το απόγευμα"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "χθες το βράδυ"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "αυτό το σαββατοκύριακο"
             , "αυτό το σκ"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ "δευτέρα το πρωί"
             , "δευτέρα πρωί"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 9, 0, 0)) Hour)
             [ "Δευτέρα νωρίς το πρωί"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 4, 0, 0), (2013, 2, 15, 12, 0, 0)) Hour)
             [ "15 Φεβρουαρίου το πρωί"
             , "το πρωί της 15ης Φεβρουαρίου"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 0)) Second)
             [ "τα τελευταία 2 δευτερόλεπτα"
             , "τα τελευταία 2 δεύτερα"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 1), (2013, 2, 12, 4, 30, 4)) Second)
             [ "τα επόμενα 3 δευτερόλεπτα"
             , "τα επόμενα τρία δεύτερα"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 28, 0), (2013, 2, 12, 4, 30, 0)) Minute)
             [ "τα τελευταία 2 λεπτά"
             , "τα τελευταία δύο λεπτά"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 31, 0), (2013, 2, 12, 4, 34, 0)) Minute)
             [ "τα επόμενα 3 λεπτά"
             , "τα επόμενα τρία λεπτά"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 3, 0, 0), (2013, 2, 12, 4, 0, 0)) Hour)
             [ "η τελευταία 1 ώρα"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ "οι επόμενες 3 ώρες"
             , "τις επόμενες τρεις ώρες"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "τελευταίες 2 μέρες"
             , "περασμένες 2 μέρες"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "επόμενες τρεις μέρες"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "επόμενες μερικές μέρες"
             ]
  , examples (datetimeInterval ((2013, 1, 28, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ "τελευταίες 2 εβδομάδες"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Week)
             [ "επόμενες 3 βδομάδες"
             ]
  , examples (datetimeInterval ((2012, 12, 1, 0, 0, 0), (2013, 2, 1, 0, 0, 0)) Month)
             [ "τελευταίοι 2 μήνες"
             , "περασμένοι 2 μήνες"
             ]
  , examples (datetimeInterval ((2013, 3, 1, 0, 0, 0), (2013, 6, 1, 0, 0, 0)) Month)
             [ "επόμενοι 3 μήνες"
             ]
  , examples (datetimeInterval ((2011, 1, 1, 0, 0, 0), (2013, 1, 1, 0, 0, 0)) Year)
             [ "τελευταία 2 χρόνια"
             ]
  , examples (datetimeInterval ((2014, 1, 1, 0, 0, 0), (2017, 1, 1, 0, 0, 0)) Year)
             [ "επόμενα 3 χρόνια"
             ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "13-15 Ιουλίου"
             , "13 με 15 Ιουλίου"
             , "13 Ιουλίου - 15 Ιουλίου"
             , "από τις 13-15 Ιουλίου"
             , "από τις 13 μέχρι τις 15 Ιουλίου"
             , "από τη 13η μέχρι την 15η Ιουλίου"
             ]
  , examples (datetimeInterval ((2013, 8, 8, 0, 0, 0), (2013, 8, 13, 0, 0, 0)) Day)
             [ "8 Αυγ - 12 Αυγ"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 9, 30, 0), (2013, 2, 12, 11, 1, 0)) Minute)
             [ "9:30 - 11:00"
             , "9:30 - 11:00 την Τρίτη"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 30, 0), (2013, 2, 14, 11, 1, 0)) Minute)
             [ "Πέμπτη από 9:30 μέχρι 11:00"
             , "Πέμπτη μεταξύ 9:30 και 11:00"
             , "Πέμπτη από τις 9:30 - 11:00"
             , "Πέμπτη από 9:30 μέχρι 11:00"
             , "Πέμπτη 9:30 - 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 1, 0, 0), (2013, 2, 13, 2, 31, 0)) Minute)
             [ "αύριο μεταξύ 1-2:30"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 15, 0, 0), (2013, 2, 12, 17, 0, 0)) Hour)
             [ "3-4 μμ"
             , "από 3 μέχρι 4 το απόγευμα"
             , "περίπου 3-4 μμ"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 15, 30, 0), (2013, 2, 12, 18, 1, 0)) Minute)
             [ "3:30 μέχρι 6 μμ"
             , "3:30-6 μ.μ."
             ]
  , examples (datetimeInterval ((2013, 2, 12, 8, 0, 0), (2013, 2, 12, 14, 0, 0)) Hour)
             [ "8πμ - 1μμ"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 0, 0), (2013, 2, 14, 12, 0, 0)) Hour)
             [ "Πέμπτη από  9π μέχρι 11π"
             , "αυτή την Πέμπτη 9-11πμ"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 11, 30, 0), (2013, 2, 12, 13, 31, 0)) Minute)
             [ "11:30-1:30"
             ]
  , examples (datetime (2013, 9, 21, 13, 30, 0) Minute)
             [ "1:30 μμ το Σάββατο, 21 Σεπτ"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 26, 0, 0, 0)) Second)
             [ "μέσα σε 2 βδομάδες"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 12, 14, 0, 0)) Second)
             [ "μέχρι τις 2:00μμ"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 13, 0, 0, 0)) Second)
             [ "μέχρι το τέλος της ημέρας"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 3, 1, 0, 0, 0)) Second)
             [ "μέχρι το τέλος του μήνα"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 4, 1, 0, 0, 0)) Second)
             [ "μέχρι το τέλος του επόμενου μήνα"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Minute)
             [ "4μμ CET"
             ]
  , examples (datetime (2013, 2, 14, 6, 0, 0) Minute)
             [ "Πέμπτη 8:00 GMT"
             , "Πέμπτη 8:00 gmt"
             , "Πεμ στις 8 GMT"
             , "Πεμ στις 8 gmt"
             ]
  , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
             [ "σήμερα στις 2μμ"
             , "στις 2μμ"
             ]
  , examples (datetime (2013, 2, 13, 15, 0, 0) Hour)
             [ "3μμ αύριο"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 12, 14, 0, 0) Hour)
             [ "μετά τις 2μμ"
             ]
  , examples (datetime (2013, 2, 17, 4, 0, 0) Hour)
             [ "σε 5 μέρες"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 11, 0, 0) Hour)
             [ "πριν τις 11 το πρωί"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 8, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
             [ "8πμ μέχρι τις 6"
             ]
  , examples (datetime (2013, 2, 12, 13, 30, 0) Minute)
             [ "στις 1:30μμ"
             , "1:30μμ"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "σε 15 λεπτά"
             , "σε 15'"
             ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ "10:30"
             , "περίπου 1030"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 0, 0), (2013, 2, 12, 12, 0, 0)) Hour)
             [ "σήμερα το πρωί"
             ]
  , examples (datetime (2013, 2, 25, 0, 0, 0) Day)
             [ "επόμενη δευτέρα"
             ]
  , examples (datetime (2013, 2, 12, 12, 0, 0) Hour)
             [ "στις 12μμ"
             , "το μεσημέρι"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Hour)
             [ "στις 12 πμ"
             , "τα μεσάνυχτα"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "Μάρτιος"
             , "στο Μάρτιο"
             , "το Μάρτιο"
             ]
  , examples (datetime (2013, 2, 13, 17, 0, 0) Hour)
             [ "αύριο το απόγευμα στις 5"
             , "στις 5 αύριο το απόγευμα"
             , "στις 5 αύριο απόγευμα"
             , "στις 5 μμ αύριο"
             , "αύριο στις 5 μμ"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 12, 0, 0), (2013, 2, 13, 18, 0, 0)) Hour)
             [ "αύριο το απόγευμα"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 13, 0, 0), (2013, 2, 13, 15, 0, 0)) Hour)
             [ "1μμ-2μμ αύριο"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "την 1η"
             , "την πρώτη"
             ]
  , examples (datetime (2013, 2, 12, 19, 30, 0) Minute)
             [ "στις 7:30 το βράδυ"
             ]
  , examples (datetime (2013, 2, 12, 23, 0, 0) Hour)
             [ "απόψε στις 11"
             ]
  , examples (datetime (2013, 2, 12, 4, 23, 0) Minute)
    -- yes, the result is in the past, we may need to revisit
             [ "στις 4:23"
             , "4:23πμ"
             ]
  , examples (datetimeInterval ((2013, 3, 1, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Day)
             [ "αρχές Μάρτη"
             ]
  , examples (datetimeInterval ((2013, 3, 11, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "μέσα Μάρτη"
             ]
  , examples (datetimeInterval ((2013, 3, 21, 0, 0, 0), (2013, 4, 1, 0, 0, 0)) Day)
             [ "τέλη Μάρτη"
             ]
  , examples (datetimeInterval ((2013, 10, 25, 18, 0, 0), (2013, 10, 28, 0, 0, 0)) Hour)
             [ "τελευταίο σαββατοκύριακο του Οκτωβρίου"
             ]
  , examples (datetimeInterval ((2013, 7, 26, 18, 0, 0), (2013, 7, 29, 0, 0, 0)) Hour)
             [ "τελευταίο ΣΚ του Ιουλίου"
             ]
  , examples (datetimeInterval ((2017, 10, 27, 18, 0, 0), (2017, 10, 30, 0, 0, 0)) Hour)
             [ "τελευταίο σκ του Οκτ 2017"
             ]
  , examples (datetimeInterval ((2013, 8, 27, 0, 0, 0), (2013, 8, 30, 0, 0, 0)) Day)
             [ "27η - 29η Αυγούστου"
             ]
  , examples (datetimeInterval ((2013, 10, 23, 0, 0, 0), (2013, 10, 27, 0, 0, 0)) Day)
             [ "23η με 26η Οκτ"
             ]
  , examples (datetimeInterval ((2013, 9, 1, 0, 0, 0), (2013, 9, 9, 0, 0, 0)) Day)
             [ "1-8 Σεπτεμβρίου"
             ]
  , examples (datetimeInterval ((2013, 9, 12, 0, 0, 0), (2013, 9, 17, 0, 0, 0)) Day)
             [ "12 με 16 Σεπτ"
             ]
  , examples (datetimeInterval ((2013, 8, 19, 0, 0, 0), (2013, 8, 22, 0, 0, 0)) Day)
             [ "19η με 21η αυγ"
             ]
  ]
