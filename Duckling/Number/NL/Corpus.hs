-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.NL.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Number.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = NL}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumberValue 0)
             [ "0"
             , "nul"
             , "geen"
             , "niks"
             ]
  , examples (NumberValue 1)
             [ "1"
             , "een"
             , "één"
             ]
  , examples (NumberValue 2)
             [ "2"
             , "twee"
             ]
  , examples (NumberValue 33)
             [ "33"
             , "3 en 30"
             , "drieendertig"
             , "drieëndertig"
             , "drie en dertig"
             , "0033"
             ]
  , examples (NumberValue 14)
             [ "14"
             , "veertien"
             ]
  , examples (NumberValue 16)
             [ "16"
             , "zestien"
             ]
  , examples (NumberValue 17)
             [ "17"
             , "zeventien"
             ]
  , examples (NumberValue 18)
             [ "18"
             , "achtien"
             ]
  , examples (NumberValue 1.1)
             [ "1,1"
             , "1,10"
             , "01,10"
             ]
  , examples (NumberValue 0.77)
             [ "0,77"
             , ",77"
             ]
  , examples (NumberValue 300)
             [ "3 honderd"
             , "drie honderd"
             ]
  , examples (NumberValue 5000)
             [ "5 duizend"
             , "vijf duizend"
             ]
  , examples (NumberValue 122)
             [ "honderd tweeëntwintig"
             , "honderd tweeentwintig"
             , "honderd twee en twintig"
             ]
  , examples (NumberValue 20000)
             [ "twintig duizend"
             ]
  ]
