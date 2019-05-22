-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE NoRebindableSyntax #-}


module Duckling.Ranking.Types
  ( Feature
  , BagOfFeatures
  , Class
  , Datum
  , Dataset

  , Classifier(..)
  , Classifiers
  , ClassData(..)

  , infinity
  ) where
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Prelude

-- -----------------------------------------------------------------
-- Aliases

type Feature = Text
type BagOfFeatures = HashMap Feature Int
type Class = Bool
type Datum = (BagOfFeatures, Class)
type Dataset = HashMap Text [Datum]

-- -----------------------------------------------------------------
-- Classification

data Classifier = Classifier
  { okData :: ClassData
  , koData :: ClassData
  }
  deriving (Eq, Show)

type Classifiers = HashMap Text Classifier

data ClassData = ClassData
  { prior :: Double
  , unseen :: Double
  , likelihoods :: HashMap Feature Double
  , n :: Int
  }
  deriving (Eq, Show)

infinity :: Double
infinity = 1 / 0
