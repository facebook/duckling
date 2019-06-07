-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.KO.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale KO Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "영"
             , "빵"
             , "공"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "일"
             , "하나"
             , "한"
             ]
  , examples (NumeralValue 10)
             [ "10"
             , "십"
             , "열"
             ]
  , examples (NumeralValue 11)
             [ "11"
             , "십일"
             , "열하나"
             , "십하나"
             , "열한"
             ]
  , examples (NumeralValue 20)
             [ "20"
             , "이십"
             , "스물"
             ]
  , examples (NumeralValue 35)
             [ "35"
             , "삼십오"
             , "서른다섯"
             ]
  , examples (NumeralValue 47)
             [ "47"
             , "사십칠"
             , "마흔일곱"
             ]
  , examples (NumeralValue 52)
             [ "52"
             , "오십이"
             , "쉰둘"
             , "쉰두"
             ]
  , examples (NumeralValue 69)
             [ "69"
             , "육십구"
             , "예순아홉"
             ]
  , examples (NumeralValue 71)
             [ "71"
             , "칠십일"
             , "일흔하나"
             , "일흔한"
             ]
  , examples (NumeralValue 84)
             [ "84"
             , "팔십사"
             , "여든넷"
             ]
  , examples (NumeralValue 93)
             [ "93"
             , "구십삼"
             , "아흔셋"
             ]
  , examples (NumeralValue 100)
             [ "100"
             , "백"
             ]
  , examples (NumeralValue 123)
             [ "123"
             , "백이십삼"
             ]
  , examples (NumeralValue 579)
             [ "579"
             , "오백칠십구"
             ]
  , examples (NumeralValue 1000)
             [ "1000"
             , "천"
             ]
  , examples (NumeralValue 1723)
             [ "1723"
             , "천칠백이십삼"
             ]
  , examples (NumeralValue 5619)
             [ "5619"
             , "오천육백십구"
             ]
  , examples (NumeralValue 10000)
             [ "10000"
             , "만"
             , "일만"
             ]
  , examples (NumeralValue 12345)
             [ "12345"
             , "만이천삼백사십오"
             , "일만이천삼백사십오"
             ]
  , examples (NumeralValue 58194)
             [ "58194"
             , "오만팔천백구십사"
             ]
  , examples (NumeralValue 581900)
             [ "581900"
             , "오십팔만천구백"
             ]
  , examples (NumeralValue 5819014)
             [ "5819014"
             , "오백팔십일만구천십사"
             ]
  , examples (NumeralValue 58190148)
             [ "58190148"
             , "오천팔백십구만백사십팔"
             ]
  , examples (NumeralValue 100000000)
             [ "100000000"
             , "일억"
             ]
  , examples (NumeralValue 274500000000)
             [ "274500000000"
             , "이천칠백사십오억"
             ]
  , examples (NumeralValue 100000002)
             [ "100000002"
             , "일억이"
             ]
  , examples (NumeralValue 27350000)
             [ "27350000"
             , "이천칠백삼십오만"
             ]
  , examples (NumeralValue 3235698120)
             [ "3235698120"
             , "삼십이억삼천오백육십구만팔천백이십"
             ]
  , examples (NumeralValue 40234985729)
             [ "40234985729"
             , "사백이억삼천사백구십팔만오천칠백이십구"
             ]
  , examples (NumeralValue 701239801123)
             [ "701239801123"
             , "칠천십이억삼천구백팔십만천백이십삼"
             ]
  , examples (NumeralValue 3.4)
             [ "3.4"
             , "삼점사"
             ]
  , examples (NumeralValue 4123.3)
             [ "4123.3"
             , "사천백이십삼점삼"
             ]
  , examples (NumeralValue 1.23)
             [ "일점이삼"
             ]
  , examples (NumeralValue (-3))
             [ "-3"
             , "마이너스3"
             , "마이너스삼"
             , "마이너스 3"
             , "마이나스3"
             , "마이나스 3"
             ]
  , examples (NumeralValue 0.75)
             [ "3/4"
             , "사분의삼"
             ]
  ]
