-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.PhoneNumber.AR.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.PhoneNumber.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus =
  (testContext { locale = makeLocale AR Nothing }, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext, testOptions, xs)
  where
    xs =
      [ "١٢٣٤٥"
      , "١٢٣٤٥٦٧٨٩٠١٢٣٤٥٦٧٧٧٧٧٧"
      , "١٢٣٤٥٦٧٨٩٠١٢٣٤٥٦"
      ]

-- Tests include both unicode characters and equivalent unicode decimal code
-- representation because the Arabic phone number regex is constructed with
-- unicode decimal form.
allExamples :: [Example]
allExamples =
  concat
    [ examples (PhoneNumberValue "6507018887")
               [ "٦٥٠٧٠١٨٨٨٧"
               , "\1638\1637\1632\1639\1632\1633\1640\1640\1640\1639"
               , "٦٥٠ ٧٠١ ٨٨٨٧"
               , "\1638\1637\1632 \1639\1632\1633 \1640\1640\1640\1639"
               , "٦٥٠-٧٠١-٨٨٨٧"
               , "\1638\1637\1632-\1639\1632\1633-\1640\1640\1640\1639"
               ]
    , examples (PhoneNumberValue "(+1) 6507018887")
               [ "+١ ٦٥٠٧٠١٨٨٨٧"
               , "+\1633 \1638\1637\1632\1639\1632\1633\1640\1640\1640\1639"
               , "(+١)٦٥٠٧٠١٨٨٨٧"
               , "(+\1633)\1638\1637\1632\1639\1632\1633\1640\1640\1640\1639"
               , "(+١)   ٦٥٠ - ٧٠١  ٨٨٨٧"
               , "(+\1633)    \1638\1637\1632 - \1639\1632\1633  \1640\1640\1640\1639"
               ]
    , examples (PhoneNumberValue "(+33) 146647998")
               [ "+٣٣ ١ ٤٦٦٤٧٩٩٨"
               , "+\1635\1635 \1633 \1636\1638\1638\1636\1639\1641\1641\1640"
               ]
    , examples (PhoneNumberValue "0620702220")
               [ "٠٦ ٢٠٧٠ ٢٢٢٠"
               ]
    , examples (PhoneNumberValue "6507018887 ext 897")
               [ "٦٥٠٧٠١٨٨٨٧ ext ٨٩٧"
               , "٦٥٠٧٠١٨٨٨٧ x ٨٩٧"
               , "٦٥٠٧٠١٨٨٨٧ ext. ٨٩٧"
               ]
    , examples (PhoneNumberValue "6507018887 ext 897")
               [ "٦٥٠٧٠١٨٨٨٧ فرعي ٨٩٧"
               ]
    , examples (PhoneNumberValue "(+1) 2025550121")
               [ "+١-٢٠٢-٥٥٥-٠١٢١"
               , "+١ ٢٠٢.٥٥٥.٠١٢١"
               ]
    , examples (PhoneNumberValue "4866827")
               [ "٤.٨.٦.٦.٨.٢.٧"
               ]
    , examples (PhoneNumberValue "(+55) 19992842606")
               [ "(+٥٥) ١٩٩٩٢٨٤٢٦٠٦"
               ]
    ]
