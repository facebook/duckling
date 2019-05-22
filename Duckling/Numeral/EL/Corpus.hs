-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.EL.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale EL Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "μηδέν"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "ένα"
             ]
  , examples (NumeralValue 2)
             [ "δύο"
             ]
  , examples (NumeralValue 3)
             [ "τρία"
             ]
  , examples (NumeralValue 4)
             [ "τέσσερα"
             ]
  , examples (NumeralValue 5)
             [ "πέντε"
             ]
  , examples (NumeralValue 6)
             [ "έξι"
             ]
  , examples (NumeralValue 7)
             [ "επτά"
             , "εφτά"
             ]
  , examples (NumeralValue 8)
             [ "οκτώ"
             , "οχτώ"
             ]
  , examples (NumeralValue 9)
             [ "εννιά"
             , "εννέα"
             ]
  , examples (NumeralValue 10)
             [ "δέκα"
             , "δεκαριά"
             ]
  , examples (NumeralValue 11)
             [ "έντεκα"
             , "ένδεκα"
             ]
  , examples (NumeralValue 15)
             [ "δεκαπέντε"
             ]
  , examples (NumeralValue 20)
             [ "20"
             , "είκοσι"
             ]
  , examples (NumeralValue 50)
             [ "πενήντα"
             ]
  , examples (NumeralValue 33)
            [ "33"
            , "τριάντα τρία"
            , "τριάντα τρεις"
            ]
  , examples (NumeralValue 24)
            [ "24"
            , "είκοσι τέσσερα"
            , "είκοσι τέσσερις"
            ]
  , examples (NumeralValue 0.77)
            [ "μηδέν κόμμα εβδομήντα επτά"
            , "0,77"
            ]
  , examples (NumeralValue 200032)
            [ "διακόσιες χιλιάδες τριάντα δύο"
            ]
  , examples (NumeralValue 7200032.356)
            [ "εφτά εκατομμύρια διακόσιες χιλιάδες τριάντα δύο κόμμα τριακόσια πενήντα έξι"
            , "7200032,356"
            , "7.200.032,356"
            ]
  , examples (NumeralValue 100000)
            [ "100000"
            , "100.000"
            , "εκατό χιλιάδες"
            ]
  , examples (NumeralValue 3e6)
            [ "τρία εκατομμύρια"
            , "3 εκατομμύρια"
            ]
  , examples (NumeralValue 1.2e6)
            [ "ένα εκατομμύριο διακόσιες χιλιάδες"
            , "1.200.000"
            , "1200000"
            ]
  , examples (NumeralValue 5000)
            [ "5 χιλιάδες"
            , "πέντε χιλιάδες"
            ]
  , examples (NumeralValue 122)
            [ "εκατόν είκοσι δύο"
            ]
  , examples (NumeralValue 743)
            [ "εφτακόσιοι σαράντα τρεις"
            , "εφτακόσια σαράντα τρία"
            , "εφτακόσιες σαράντα τρεις"
            ]
  , examples (NumeralValue 398)
            [ "τριακόσια ενενήντα οχτώ"
            ]
  , examples (NumeralValue 2e5)
            [ "διακόσιες χιλιάδες"
            ]
  , examples (NumeralValue 21011)
            [ "είκοσι μία χιλιάδες έντεκα"
            ]
  , examples (NumeralValue 721012)
            [ "εφτακόσιες είκοσι μία χιλιάδες δώδεκα"
            ]
  , examples (NumeralValue 31256721)
            [ "τριάντα ένα εκατομμύρια διακόσιες πενήντα έξι χιλιάδες εφτακόσια είκοσι ένα"
            , "31.256.721"
            ]
  , examples (NumeralValue 2400)
            [ "δύο χιλιάδες τετρακόσια"
            ]
  , examples (NumeralValue 2200000)
            [ "δύο κόμμα δύο εκατομμύρια"
            , "2,2 εκατομμύρια"
            ]
  ]
