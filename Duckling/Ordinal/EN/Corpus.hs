-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.EN.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Ordinal.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 1)
             [ "first"
             , "1st"
             ]
  , examples (OrdinalData 2)
             [ "second"
             , "2nd"
             ]
  , examples (OrdinalData 3)
             [ "third"
             , "3rd"
             ]
  , examples (OrdinalData 4)
             [ "fourth"
             , "4th"
             ]
  , examples (OrdinalData 8)
             [ "eighth"
             , "8th"
             ]
  , examples (OrdinalData 25)
             [ "twenty-fifth"
             , "twenty—fifth"
             , "twenty fifth"
             , "twentyfifth"
             , "25th"
             ]
  , examples (OrdinalData 31)
             [ "thirty-first"
             , "thirty—first"
             , "thirty first"
             , "thirtyfirst"
             , "31st"
             ]
  , examples (OrdinalData 42)
             [ "forty-second"
             , "forty—second"
             , "forty second"
             , "fortysecond"
             , "42nd"
             ]
  , examples (OrdinalData 73)
            [ "seventy-third"
            , "seventy—third"
            , "seventy third"
            , "seventythird"
            , "73rd"
            ]
  , examples (OrdinalData 90)
            [ "ninetieth"
            , "90th"
            ]
  ]
