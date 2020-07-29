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
    examples (simple 0)
             [ "0"
             , "ไม่มี"
             , "ศูนย์"
             ]
   ,examples (simple 1)
             [ "1"
             , "หนึ่ง"
             ]
  , examples (simple 2)
             [ "2"
             , "สอง"
             ]
  , examples (simple 3)
             [ "3"
             , "สาม"
             ]
  , examples (simple 10)
             [ "10"
             , "สิบ"
             ]
  , examples (simple 12)
             [ "12"
             , "สิบสอง"
             , "โหล"
             ]
  , examples (simple 14)
             [ "14"
             , "สิบสี่"
             ]
  , examples (simple 16)
             [ "16"
             , "สิบหก"
             ]
  , examples (simple 17)
             [ "17"
             , "สิบเจ็ด"
             ]
  , examples (simple 18)
             [ "18"
             , "สิบแปด"
             ]
  , examples (simple 33)
             [ "33"
             , "0033"
             ]
  , examples (simple 24)
             [ "24"
             , "สองโหล"
             ]
  , examples (simple 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             , "1 จุด 1"
             , "หนึ่งจุดหนึ่ง"
             ]
  , examples (simple 0.77)
             [ ".77"
             , "0.77"
             , "จุด 77"
             ]
  , examples (simple 100000)
             [ "100,000"
             , "100,000.0"
             , "100000"
             , "หนึ่งแสน"
             ]
  , examples (simple 0.2)
             [ "1/5"
             , "2/10"
             , "3/15"
             , "20/100"
             , "ศูนย์จุดสอง"
             ]
  , examples (simple 3e6)
             [ "3000000"
             , "3,000,000"
             , "3 ล้าน"
             , "สามล้าน"
             ]
  , examples (simple 1.2e6)
             [ "1,200,000"
             , "1200000"
             , "1.2 ล้าน"
             , "หนึ่งจุดสองล้าน"
             ]
  , examples (simple 5000)
             [ "ห้าพัน"
             ]
  , examples (simple (-504))
             [ "-504"
             ]
  , examples (simple (-1.2e6))
             [ "- 1,200,000"
             , "-1200000"
             , "ลบ 1,200,000"
             ]
  , examples (simple 122)
             [ "หนึ่งร้อยยี่สิบสอง"
             ]
  , examples (simple 2e5)
             [ "สองแสน"
             ]
  , examples (simple 21011)
             [ "สองหมื่นหนึ่งพันสิบเอ็ด"
             , "สองหมื่นหนึ่งพันสิบหนึ่ง"
             ]
  , examples (simple 721012)
             [ "เจ็ดแสนสองหมื่นหนึ่งพันสิบสอง"
             ]
  , examples (simple 31256721)
             [ "สามสิบเอ็ดล้านสองแสนห้าหมื่นหกพันเจ็ดร้อยยี่สิบเอ็ด"
             , "สามสิบหนึ่งล้านสองแสนห้าหมื่นหกพันเจ็ดร้อยยี่สิบหนึ่ง"
             ]
  , examples (simple 2400)
             [ "สองร้อยโหล"
             , "200 โหล"
             ]
  , examples (simple 2200000)
             [ "สองจุดสองล้าน"
             , "สองล้านสองแสน"
             ]
  , examples (simple 3000000000)
             [ "สามพันล้าน"
             ]
  ]
