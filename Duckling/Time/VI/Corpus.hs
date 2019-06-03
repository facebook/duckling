-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.VI.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month, refTime)
import Duckling.TimeGrain.Types hiding (add)

negativeCorpus :: NegativeCorpus
negativeCorpus = (context, testOptions, examples)
  where
    examples =
      [ "có ngày chính xác"
      ]

corpus :: Corpus
corpus = (context, testOptions, allExamples)

context :: Context
context = testContext
  { locale = makeLocale VI Nothing
  , referenceTime = refTime (2017, 2, 2, 3, 55, 0) (-2)
  }

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2017, 2, 2, 3, 55, 0) Second)
             [ "bây giờ"
             , "ngay bây giờ"
             , "ngay lúc này"
             ]
  , examples (datetime (2017, 2, 2, 0, 0, 0) Day)
             [ "hôm nay"
             , "ngày hôm nay"
             , "bữa nay"
             ]
  , examples (datetime (2017, 2, 1, 0, 0, 0) Day)
             [ "hôm qua"
             , "ngày hôm qua"
             ]
  , examples (datetime (2017, 2, 3, 0, 0, 0) Day)
             [ "ngày mai"
             ]
  , examples (datetime (2017, 1, 31, 0, 0, 0) Day)
             [ "hôm kia"
             , "ngày hôm kia"
             ]
  , examples (datetime (2017, 2, 6, 0, 0, 0) Day)
             [ "thứ 2"
             , "thứ hai"
             ]
  , examples (datetime (2017, 2, 6, 0, 0, 0) Day)
             [ "thứ 2 ngày 6 tháng 2"
             , "thứ 2 mồng 6 tháng 2"
             , "thứ hai ngày 6 tháng 2"
             ]
  , examples (datetime (2017, 2, 7, 0, 0, 0) Day)
             [ "thứ 3"
             , "thứ ba"
             ]
  , examples (datetime (2017, 2, 5, 0, 0, 0) Day)
             [ "chủ nhật"
             ]
  , examples (datetime (2017, 6, 1, 0, 0, 0) Month)
             [ "tháng 6"
             , "tháng sáu"
             ]
  , examples (datetime (2017, 3, 1, 0, 0, 0) Day)
             [ "ngày đầu tiên của tháng ba"
             , "ngày đầu tiên của tháng 3"
             ]
  , examples (datetime (2017, 3, 3, 0, 0, 0) Day)
             [ "mồng 3 tháng ba"
             , "mồng 3 tháng 3"
             ]
  , examples (datetime (2017, 3, 3, 0, 0, 0) Day)
             [ "ngày mồng 3 tháng 3 năm 2017"
             , "ngày 3 tháng 3 năm 2017"
             , "3/3/2017"
             , "3/3/17"
             , "03/03/2017"
             ]
  , examples (datetime (2017, 3, 7, 0, 0, 0) Day)
             [ "ngày mồng 7 tháng 3"
             , "ngày 7 tháng ba"
             , "7/3"
             , "07/03"
             ]
  , examples (datetime (2017, 10, 1, 0, 0, 0) Month)
             [ "tháng 10 năm 2017"
             , "tháng mười năm 2017"
             ]
  , examples (datetime (1991, 9, 3, 0, 0, 0) Day)
             [ "03/09/1991"
             , "3/9/91"
             , "3/9/1991"
             ]
  , examples (datetime (2017, 10, 12, 0, 0, 0) Day)
             [ "12 tháng 10 năm 2017"
             , "ngày 12 tháng 10 năm 2017"
             ]
  , examples (datetime (2017, 2, 9, 0, 0, 0) Day)
             [ "thứ năm tuần tới"
             , "thứ 5 tuần sau"
             ]
  , examples (datetime (2017, 3, 1, 0, 0, 0) Month)
             [ "tháng 3 tới"
             ]
  , examples (datetime (2017, 4, 9, 0, 0, 0) Day)
             [ "chủ nhật ngày mồng 9 tháng 4"
             , "chủ nhật ngày 9 tháng 4"
             ]
  , examples (datetime (2017, 2, 6, 0, 0, 0) Day)
             [ "thứ 2 ngày 6 tháng 2"
             , "thứ 2 ngày mồng 6 tháng 2"
             , "thứ hai ngày mồng 6 tháng 2"
             ]
  , examples (datetime (2018, 4, 3, 0, 0, 0) Day)
             [ "thứ 3 ngày 3 tháng 4 năm 2018"
             ]
  , examples (datetime (2017, 1, 30, 0, 0, 0) Week)
             [ "tuần này"
             ]
  , examples (datetime (2017, 1, 23, 0, 0, 0) Week)
             [ "tuần trước"
             ]
  , examples (datetime (2017, 2, 6, 0, 0, 0) Week)
             [ "tuần sau"
             ]
  , examples (datetime (2017, 1, 1, 0, 0, 0) Month)
             [ "tháng trước"
             ]
  , examples (datetime (2017, 3, 1, 0, 0, 0) Month)
             [ "tháng sau"
             ]
  , examples (datetime (2017, 1, 1, 0, 0, 0) Quarter)
             [ "quý này"
             ]
  , examples (datetime (2017, 4, 1, 0, 0, 0) Quarter)
             [ "quý sau"
             ]
  , examples (datetime (2017, 7, 1, 0, 0, 0) Quarter)
             [ "quý 3"
             , "quý ba"
             ]
  , examples (datetime (2018, 10, 1, 0, 0, 0) Quarter)
             [ "quý 4 năm 2018"
             ]
  , examples (datetime (2016, 1, 1, 0, 0, 0) Year)
             [ "năm trước"
             , "năm ngoái"
             ]
  , examples (datetime (2017, 1, 1, 0, 0, 0) Year)
             [ "năm nay"
             ]
  , examples (datetime (2018, 1, 1, 0, 0, 0) Year)
             [ "năm sau"
             ]
  , examples (datetime (2017, 1, 1, 0, 0, 0) Quarter)
             [ "quý này"
             , "quý nay"
             , "quý hiện tại"
             ]
  , examples (datetime (2017, 4, 1, 0, 0, 0) Quarter)
             [ "quý tới"
             , "quý tiếp"
             ]
  , examples (datetime (2017, 7, 1, 0, 0, 0) Quarter)
             [ "quý ba"
             , "quý 3"
             ]
  , examples (datetime (2018, 10, 1, 0, 0, 0) Quarter)
             [ "quý 4 của năm 2018"
             ]
  , examples (datetime (2016, 1, 1, 0, 0, 0) Year)
             [ "năm ngoái"
             , "năm trước"
             ]
  , examples (datetime (2017, 1, 1, 0, 0, 0) Year)
             [ "năm nay"
             ]
  , examples (datetime (2018, 1, 1, 0, 0, 0) Year)
             [ "năm tiếp theo"
             , "năm kế tiếp"
             , "năm tới"
             ]
  , examples (datetime (2017, 1, 31, 0, 0, 0) Day)
             [ "thứ ba vừa rồi"
             ]
  , examples (datetime (2017, 2, 7, 0, 0, 0) Day)
             [ "thứ ba tới"
             ]
  , examples (datetime (2017, 2, 3, 0, 0, 0) Day)
             [ "thứ sáu tới"
             ]
  , examples (datetime (2017, 2, 8, 0, 0, 0) Day)
             [ "thứ tư tuần tới"
             , "thứ tư của tuần tới"
             ]
  , examples (datetime (2017, 2, 3, 0, 0, 0) Day)
             [ "thứ sáu tuần này"
             , "thứ 6 tuần này"
             , "thứ 6 của tuần này"
             ]
  , examples (datetime (2017, 2, 2, 0, 0, 0) Day)
             [ "thứ năm tuần này"
             , "thứ 5 của tuần này"
             ]
  , examples (datetime (2017, 9, 4, 0, 0, 0) Week)
             [ "tuần đầu tiên của tháng 9 năm 2017"
             ]
  , examples (datetime (2017, 2, 3, 2, 0, 0) Hour)
             [ "vào lúc 2 giờ sáng"
             , "lúc 2 giờ sáng"
             ]
  , examples (datetime (2017, 2, 3, 1, 18, 0) Minute)
             [ "1:18 sáng"
             ]
  , examples (datetime (2017, 2, 2, 15, 0, 0) Hour)
             [ "lúc 3 giờ tối"
             , "vào lúc 3 giờ chiều"
             , "vào đúng 3 giờ chiều"
             ]
  , examples (datetime (2017, 2, 2, 15, 0, 0) Hour)
             [ "vào khoảng 3 giờ chiều"
             , "khoảng 3 giờ chiều"
             ]
  , examples (datetime (2017, 2, 2, 15, 30, 0) Minute)
             [ "3 giờ rưỡi chiều"
             , "3:30 chiều"
             , "ba giờ rưỡi chiều"
             ]
  , examples (datetime (2017, 2, 2, 14, 30, 0) Minute)
             [ "2:30"
             , "hai giờ rưỡi"
             ]
  , examples (datetime (2017, 2, 2, 15, 23, 24) Second)
             [ "15:23:24"
             ]
  , examples (datetime (2017, 2, 2, 10, 45, 0) Minute)
             [ "11 giờ kém 15"
             , "10 giờ 45 phút"
             , "10:45"
             , "10 giờ 45"
             , "10h45"
             , "10g45"
             ]
  , examples (datetime (2017, 2, 2, 20, 0, 0) Hour)
             [ "8 giờ tối nay"
             ]
  , examples (datetime (2017, 4, 20, 19, 30, 0) Minute)
             [ "vào lúc 7:30 chiều ngày 20 tháng 4 năm 2017"
             , "7:30 chiều ngày 20/4/2017"
             ]
  , examples (datetimeInterval ((2017, 6, 21, 0, 0, 0), (2017, 9, 24, 0, 0, 0)) Day)
             [ "mùa hè này"
             , "mùa hè năm nay"
             ]
  , examples (datetimeInterval ((2016, 12, 21, 0, 0, 0), (2017, 3, 21, 0, 0, 0)) Day)
             [ "mùa đông này"
             ]
  , examples (datetimeInterval ((2017, 2, 2, 18, 0, 0), (2017, 2, 3, 0, 0, 0)) Hour)
             [ "tối nay"
             , "tối hôm nay"
             ]
  , examples (datetimeInterval ((2017, 2, 3, 18, 0, 0), (2017, 2, 4, 0, 0, 0)) Hour)
             [ "tối mai"
             , "tối ngày mai"
             ]
  , examples (datetimeInterval ((2017, 2, 3, 12, 0, 0), (2017, 2, 3, 14, 0, 0)) Hour)
             [ "trưa mai"
             , "trưa ngày mai"
             ]
  , examples (datetimeInterval ((2017, 2, 1, 18, 0, 0), (2017, 2, 2, 0, 0, 0)) Hour)
             [ "tối qua"
             , "tối hôm qua"
             ]
  , examples (datetimeInterval ((2017, 2, 5, 4, 0, 0), (2017, 2, 5, 12, 0, 0)) Hour)
             [ "sáng chủ nhật"
             ]
  , examples (datetimeInterval ((2017, 2, 2, 3, 54, 58), (2017, 2, 2, 3, 55, 0)) Second)
             [ "2 giây vừa rồi"
             ]
  , examples (datetimeInterval ((2017, 2, 2, 3, 55, 1), (2017, 2, 2, 3, 55, 4)) Second)
             [ "3 giây tới"
             , "3 giây tiếp theo"
             , "3 s tiếp theo"
             ]
  , examples (datetimeInterval ((2017, 2, 2, 3, 53, 0), (2017, 2, 2, 3, 55, 0)) Minute)
             [ "2 phút vừa rồi"
             ]
  , examples (datetimeInterval ((2017, 2, 2, 3, 56, 0), (2017, 2, 2, 3, 59, 0)) Minute)
             [ "3 phút tới"
             , "3 phút tiếp theo"
             ]
  , examples (datetimeInterval ((2017, 2, 2, 2, 0, 0), (2017, 2, 2, 3, 0, 0)) Hour)
             [ "một tiếng vừa rồi"
             , "1 giờ vừa qua"
             ]
  , examples (datetimeInterval ((2017, 2, 2, 4, 0, 0), (2017, 2, 2, 7, 0, 0)) Hour)
             [ "3 tiếng tiếp theo"
             , "3 giờ tới"
             ]
  , examples (datetimeInterval ((2017, 1, 31, 0, 0, 0), (2017, 2, 2, 0, 0, 0)) Day)
             [ "2 ngày vừa rồi"
             , "2 ngày vừa qua"
             ]
  , examples (datetimeInterval ((2017, 2, 3, 0, 0, 0), (2017, 2, 6, 0, 0, 0)) Day)
             [ "3 ngày tới"
             , "3 ngày tiếp theo"
             ]
  , examples (datetimeInterval ((2016, 12, 1, 0, 0, 0), (2017, 2, 1, 0, 0, 0)) Month)
             [ "2 tháng vừa rồi"
             , "2 tháng qua"
             ]
  , examples (datetimeInterval ((2017, 3, 1, 0, 0, 0), (2017, 6, 1, 0, 0, 0)) Month)
             [ "3 tháng tới"
             , "ba tháng tiếp theo"
             ]
  , examples (datetimeInterval ((2015, 1, 1, 0, 0, 0), (2017, 1, 1, 0, 0, 0)) Year)
             [ "2 năm vừa rồi"
             ]
  , examples (datetimeInterval ((2018, 1, 1, 0, 0, 0), (2021, 1, 1, 0, 0, 0)) Year)
             [ "3 năm tới"
             , "3 năm tiếp theo"
             ]
  , examples (datetime (2017, 2, 2, 13, 0, 0) Minute)
             [ "4pm CET"
             ]
  , examples (datetime (2017, 2, 2, 14, 0, 0) Hour)
             [ "hôm nay lúc 2 giờ chiều"
             , "lúc 2 giờ chiều"
             ]
  , examples (datetime (2017, 4, 23, 16, 0, 0) Minute)
             [ "lúc 4:00 chiều ngày 23/4"
             ]
  , examples (datetime (2017, 10, 12, 0, 0, 0) Day)
             [ "ngày 12/10"
             ]
  , examples (datetime (2017, 4, 23, 16, 0, 0) Hour)
             [ "lúc 4 giờ chiều ngày 23 tháng 4"
             ]
  , examples (datetime (2017, 2, 3, 15, 0, 0) Hour)
             [ "3 giờ chiều ngày mai"
             ]
  , examples (datetime (2017, 2, 2, 13, 30, 0) Minute)
             [ "lúc 1:30 chiều"
             , "lúc 1 giờ 30 chiều"
             ]
  , examples (datetimeInterval ((2017, 2, 2, 13, 0, 0), (2017, 2, 2, 17, 0, 0)) Hour)
             [ "sau bữa trưa"
             ]
  , examples (datetime (2017, 2, 2, 10, 30, 0) Minute)
             [ "10:30"
             ]
  , examples (datetimeInterval ((2017, 2, 2, 4, 0, 0), (2017, 2, 2, 12, 0, 0)) Hour)
             [ "buổi sáng nay"
             ]
  , examples (datetime (2017, 2, 6, 0, 0, 0) Day)
             [ "thứ hai tới"
             , "thứ 2 tới"
             ]
  , examples (datetime (2017, 4, 1, 0, 0, 0) Month)
             [ "tháng 4"
             , "tháng tư"
             ]
  , examples (datetime (2017, 12, 25, 0, 0, 0) Day)
             [ "giáng sinh"
             , "ngày giáng sinh"
             ]
  ]
