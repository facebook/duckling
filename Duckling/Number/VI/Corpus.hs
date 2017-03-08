-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.VI.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Number.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = VI}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumberValue 0)
             [ "0"
             , "không"
             ]
  , examples (NumberValue 1)
             [ "1"
             , "một"
             ]
  , examples (NumberValue 2)
             [ "2"
             , "hai"
             ]
  , examples (NumberValue 33)
             [ "33"
             , "ba mươi ba"
             ]
  , examples (NumberValue 14)
             [ "14"
             , "mười bốn"
             ]
  , examples (NumberValue 16)
             [ "16"
             , "mười sáu"
             ]
  , examples (NumberValue 17)
             [ "17"
             , "mười bảy"
             ]
  , examples (NumberValue 18)
             [ "18"
             , "mười tám"
             ]
  , examples (NumberValue 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             ]
  , examples (NumberValue 0.77)
             [ "0.77"
             , ".77"
             ]
  , examples (NumberValue 100000)
             [ "100,000"
             , "100000"
             , "100K"
             , "100k"
             ]
  , examples (NumberValue 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3,000,000"
             ]
  , examples (NumberValue 1200000)
             [ "1,200,000"
             , "1200000"
             , "1.2M"
             , "1200K"
             , ".0012G"
             ]
  , examples (NumberValue (-1200000))
             [ "- 1,200,000"
             , "-1200000"
             , "âm 1,200,000"
             , "-1.2M"
             , "-1200K"
             , "-.0012G"
             ]
  , examples (NumberValue 5000)
             [ "5 nghìn"
             , "năm nghìn"
             ]
  , examples (NumberValue 200000)
             [ "hai trăm nghìn"
             ]
  , examples (NumberValue 1000000000)
             [ "một tỷ"
             ]
  , examples (NumberValue 21011)
             [ "hai mươi mốt nghìn không trăm mười một"
             ]
  , examples (NumberValue 721012)
             [ "bảy trăm hai mươi mốt nghìn không trăm mười hai"
             ]
  ]
