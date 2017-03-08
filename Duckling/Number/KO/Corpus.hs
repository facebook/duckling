-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.KO.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Number.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = KO}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumberValue 0)
             [ "0"
             , "영"
             , "빵"
             , "공"
             ]
  , examples (NumberValue 1)
             [ "1"
             , "일"
             , "하나"
             , "한"
             ]
  , examples (NumberValue 10)
             [ "10"
             , "십"
             , "열"
             ]
  , examples (NumberValue 11)
             [ "11"
             , "십일"
             , "열하나"
             , "십하나"
             , "열한"
             ]
  , examples (NumberValue 20)
             [ "20"
             , "이십"
             , "스물"
             ]
  , examples (NumberValue 35)
             [ "35"
             , "삼십오"
             , "서른다섯"
             ]
  , examples (NumberValue 47)
             [ "47"
             , "사십칠"
             , "마흔일곱"
             ]
  , examples (NumberValue 52)
             [ "52"
             , "오십이"
             , "쉰둘"
             , "쉰두"
             ]
  , examples (NumberValue 69)
             [ "69"
             , "육십구"
             , "예순아홉"
             ]
  , examples (NumberValue 71)
             [ "71"
             , "칠십일"
             , "일흔하나"
             , "일흔한"
             ]
  , examples (NumberValue 84)
             [ "84"
             , "팔십사"
             , "여든넷"
             ]
  , examples (NumberValue 93)
             [ "93"
             , "구십삼"
             , "아흔셋"
             ]
  , examples (NumberValue 100)
             [ "100"
             , "백"
             ]
  , examples (NumberValue 123)
             [ "123"
             , "백이십삼"
             ]
  , examples (NumberValue 579)
             [ "579"
             , "오백칠십구"
             ]
  , examples (NumberValue 1000)
             [ "1000"
             , "천"
             ]
  , examples (NumberValue 1723)
             [ "1723"
             , "천칠백이십삼"
             ]
  , examples (NumberValue 5619)
             [ "5619"
             , "오천육백십구"
             ]
  , examples (NumberValue 10000)
             [ "10000"
             , "만"
             , "일만"
             ]
  , examples (NumberValue 12345)
             [ "12345"
             , "만이천삼백사십오"
             , "일만이천삼백사십오"
             ]
  , examples (NumberValue 58194)
             [ "58194"
             , "오만팔천백구십사"
             ]
  , examples (NumberValue 581900)
             [ "581900"
             , "오십팔만천구백"
             ]
  , examples (NumberValue 5819014)
             [ "5819014"
             , "오백팔십일만구천십사"
             ]
  , examples (NumberValue 58190148)
             [ "58190148"
             , "오천팔백십구만백사십팔"
             ]
  , examples (NumberValue 100000000)
             [ "100000000"
             , "일억"
             ]
  , examples (NumberValue 274500000000)
             [ "274500000000"
             , "이천칠백사십오억"
             ]
  , examples (NumberValue 100000002)
             [ "100000002"
             , "일억이"
             ]
  , examples (NumberValue 27350000)
             [ "27350000"
             , "이천칠백삼십오만"
             ]
  , examples (NumberValue 3235698120)
             [ "3235698120"
             , "삼십이억삼천오백육십구만팔천백이십"
             ]
  , examples (NumberValue 40234985729)
             [ "40234985729"
             , "사백이억삼천사백구십팔만오천칠백이십구"
             ]
  , examples (NumberValue 701239801123)
             [ "701239801123"
             , "칠천십이억삼천구백팔십만천백이십삼"
             ]
  , examples (NumberValue 3.4)
             [ "3.4"
             , "삼점사"
             ]
  , examples (NumberValue 4123.3)
             [ "4123.3"
             , "사천백이십삼점삼"
             ]
  , examples (NumberValue 1.23)
             [ "일점이삼"
             ]
  , examples (NumberValue (-3))
             [ "-3"
             , "마이너스3"
             , "마이너스삼"
             , "마이너스 3"
             , "마이나스3"
             , "마이나스 3"
             ]
  , examples (NumberValue 0.75)
             [ "3/4"
             , "사분의삼"
             ]
  ]
