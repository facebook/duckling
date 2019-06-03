-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.EN.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Distance.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Kilometre 3)
             [ "3 kilometers"
             , "3 km"
             , "3km"
             , "3k"
             , "3.0 km"
             ]
  , examples (simple Mile 8)
             [ "8 miles"
             , "eight mile"
             , "8 mi"
             ]
  , examples (simple M 9)
             [ "9m"
             ]
  , examples (simple Centimetre 2)
             [ "2cm"
             , "2 centimeters"
             ]
  , examples (simple Inch 5)
             [ "5 in"
             , "5''"
             , "five inches"
             , "5\""
             ]
  , examples (simple Metre 1.87)
             [ "1.87 meters"
             ]
  -- Composite values:
  , examples (simple Inch 94)
             [ "7 feet and 10 inches"
             , "7 feet, 10 inches"
             , "7 feet 10 inches"
             ]
  , examples (simple Metre 2001)
             [ "2 km and 1 meter"
             , "2 kilometer, 1 metre"
             , "2 kilometer 1 metre"
             ]
  , examples (simple Inch 166)
             [ "2 yards 7 ft 10 inches"
             , "2 yds, 7 feet and 10 inches"
             , "2 yards, 7 feet, 10 in"
             ]
  , examples (simple Foot 13)
             [ "2 yards and 7 feet"
             , "2 yards, 7 feet"
             , "2 yd 7'"
             ]
  , examples (simple Centimetre 1000806)
             [ "10 kms 8 metres 6 cm"
             , "10 kms, 8 meters, 6 cm"
             , "10 kms, 8 meters and 6 centimeters"
--             , "10 kms, 8 meters, and 6 cm" -- Oxford comma not supported
             ]
  , examples (simple Metre 1.3048)
             [ "1 meter and 1 foot"
             ]
  , examples (simple Kilometre 2.609344)
             [ "1 kilometer and 1 mile"
             ]
  , examples (simple M 3)
             -- The original, ambiguous "m" unit is preserved
             [ "3m"
             ]
  , examples (simple Centimetre 305)
             -- The ambiguous "m" unit is inferred as "meteres"
             [ "3m and 5cm"
             ]
  , examples (simple Foot 5281)
             -- The ambiguous "m" unit is inferred as "miles"
             [ "1m and 1ft"
             ]
  -- Ranges:
  , examples (between Kilometre (3, 5))
             [ "between 3 and 5 kilometers"
             , "from 3km to 5km"
             , "around 3-5 kilometers"
             , "about 3km-5km"
             , "3-5 kilometers"
             ]
  , examples (under Mile 3.5)
             [ "under 3.5 miles"
             , "less than 3.5mi"
             , "lower than three point five miles"
             ]
  , examples (above Inch 5)
             [ "more than five inches"
             , "at least 5''"
             , "over 5\""
             , "above 5 in"
             ]
  , examples (between Millimetre (5, 6))
             [ "between 5 and six millimeters"
             , "between 5 and six millimetres"
             , "5-6 mm"
             ]
  ]
