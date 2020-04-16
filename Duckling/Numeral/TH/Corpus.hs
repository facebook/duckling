-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.TH.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale TH Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [
    examples (NumeralValue 0)
             [ "0"
             , "ไม่มี"
             , "ศูนย์"
             ]
   ,examples (NumeralValue 1)
             [ "1"
             , "หนึ่ง"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "สอง"
             ]
  , examples (NumeralValue 3)
             [ "3"
             , "สาม"
             ]
  , examples (NumeralValue 10)
             [ "10"
             , "สิบ"
             ]
  , examples (NumeralValue 12)
             [ "12"
             , "สิบสอง"
             , "โหล"
             ]
  , examples (NumeralValue 14)
             [ "14"
             , "สิบสี่"
             ]
  , examples (NumeralValue 16)
             [ "16"
             , "สิบหก"
             ]
  , examples (NumeralValue 17)
             [ "17"
             , "สิบเจ็ด"
             ]
  , examples (NumeralValue 18)
             [ "18"
             , "สิบแปด"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "0033"
             ]
  , examples (NumeralValue 24)
             [ "24"
             , "สองโหล"
             ]
  , examples (NumeralValue 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             , "1 จุด 1"
             , "หนึ่งจุดหนึ่ง"
             ]
  , examples (NumeralValue 0.77)
             [ ".77"
             , "0.77"
             , "จุด 77"
             ]
  , examples (NumeralValue 100000)
             [ "100,000"
             , "100,000.0"
             , "100000"
             , "หนึ่งแสน"
             ]
  , examples (NumeralValue 0.2)
             [ "1/5"
             , "2/10"
             , "3/15"
             , "20/100"
             , "ศูนย์จุดสอง"
             ]
  , examples (NumeralValue 3e6)
             [ "3000000"
             , "3,000,000"
             , "3 ล้าน"
             , "สามล้าน"
             ]
  , examples (NumeralValue 1.2e6)
             [ "1,200,000"
             , "1200000"
             , "1.2 ล้าน"
             , "หนึ่งจุดสองล้าน"
             ]
  , examples (NumeralValue 5000)
             [ "ห้าพัน"
             ]
  , examples (NumeralValue (-504))
             [ "-504"
             ]
  , examples (NumeralValue (-1.2e6))
             [ "- 1,200,000"
             , "-1200000"
             , "ลบ 1,200,000"
             ]
  , examples (NumeralValue 122)
             [ "หนึ่งร้อยยี่สิบสอง"
             ]
  , examples (NumeralValue 2e5)
             [ "สองแสน"
             ]
  , examples (NumeralValue 21011)
             [ "สองหมื่นหนึ่งพันสิบเอ็ด"
             , "สองหมื่นหนึ่งพันสิบหนึ่ง"
             ]
  , examples (NumeralValue 721012)
             [ "เจ็ดแสนสองหมื่นหนึ่งพันสิบสอง"
             ]
  , examples (NumeralValue 31256721)
             [ "สามสิบเอ็ดล้านสองแสนห้าหมื่นหกพันเจ็ดร้อยยี่สิบเอ็ด"
             , "สามสิบหนึ่งล้านสองแสนห้าหมื่นหกพันเจ็ดร้อยยี่สิบหนึ่ง"
             ]
  , examples (NumeralValue 2400)
             [ "สองร้อยโหล"
             , "200 โหล"
             ]
  , examples (NumeralValue 2200000)
             [ "สองจุดสองล้าน"
             , "สองล้านสองแสน"
             ]
  , examples (NumeralValue 3000000000)
             [ "สามพันล้าน"
             ]
  ]
