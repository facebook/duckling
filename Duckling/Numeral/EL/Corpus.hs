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
  [ examples (simple 0)
             [ "0"
             , "μηδέν"
             ]
  , examples (simple 1)
             [ "1"
             , "ένα"
             ]
  , examples (simple 2)
             [ "δύο"
             ]
  , examples (simple 3)
             [ "τρία"
             ]
  , examples (simple 4)
             [ "τέσσερα"
             ]
  , examples (simple 5)
             [ "πέντε"
             ]
  , examples (simple 6)
             [ "έξι"
             ]
  , examples (simple 7)
             [ "επτά"
             , "εφτά"
             ]
  , examples (simple 8)
             [ "οκτώ"
             , "οχτώ"
             ]
  , examples (simple 9)
             [ "εννιά"
             , "εννέα"
             ]
  , examples (simple 10)
             [ "δέκα"
             , "δεκαριά"
             ]
  , examples (simple 11)
             [ "έντεκα"
             , "ένδεκα"
             ]
  , examples (simple 15)
             [ "δεκαπέντε"
             ]
  , examples (simple 20)
             [ "20"
             , "είκοσι"
             ]
  , examples (simple 50)
             [ "πενήντα"
             ]
  , examples (simple 33)
            [ "33"
            , "τριάντα τρία"
            , "τριάντα τρεις"
            ]
  , examples (simple 24)
            [ "24"
            , "είκοσι τέσσερα"
            , "είκοσι τέσσερις"
            ]
  , examples (simple 0.77)
            [ "μηδέν κόμμα εβδομήντα επτά"
            , "0,77"
            ]
  , examples (simple 200032)
            [ "διακόσιες χιλιάδες τριάντα δύο"
            ]
  , examples (simple 7200032.356)
            [ "εφτά εκατομμύρια διακόσιες χιλιάδες τριάντα δύο κόμμα τριακόσια πενήντα έξι"
            , "7200032,356"
            , "7.200.032,356"
            ]
  , examples (simple 100000)
            [ "100000"
            , "100.000"
            , "εκατό χιλιάδες"
            ]
  , examples (simple 3e6)
            [ "τρία εκατομμύρια"
            , "3 εκατομμύρια"
            ]
  , examples (simple 1.2e6)
            [ "ένα εκατομμύριο διακόσιες χιλιάδες"
            , "1.200.000"
            , "1200000"
            ]
  , examples (simple 5000)
            [ "5 χιλιάδες"
            , "πέντε χιλιάδες"
            ]
  , examples (simple 122)
            [ "εκατόν είκοσι δύο"
            ]
  , examples (simple 743)
            [ "εφτακόσιοι σαράντα τρεις"
            , "εφτακόσια σαράντα τρία"
            , "εφτακόσιες σαράντα τρεις"
            ]
  , examples (simple 398)
            [ "τριακόσια ενενήντα οχτώ"
            ]
  , examples (simple 2e5)
            [ "διακόσιες χιλιάδες"
            ]
  , examples (simple 21011)
            [ "είκοσι μία χιλιάδες έντεκα"
            ]
  , examples (simple 721012)
            [ "εφτακόσιες είκοσι μία χιλιάδες δώδεκα"
            ]
  , examples (simple 31256721)
            [ "τριάντα ένα εκατομμύρια διακόσιες πενήντα έξι χιλιάδες εφτακόσια είκοσι ένα"
            , "31.256.721"
            ]
  , examples (simple 2400)
            [ "δύο χιλιάδες τετρακόσια"
            ]
  , examples (simple 2200000)
            [ "δύο κόμμα δύο εκατομμύρια"
            , "2,2 εκατομμύρια"
            ]
  ]
