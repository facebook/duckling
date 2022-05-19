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
  , Candidate(..)

  , infinity
  ) where
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Prelude
import Duckling.Types
  ( Node(..)
  , Range(..)
  , ResolvedToken(..)
  , Token(..)
  , isDimension
  )

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

-- -----------------------------------------------------------------
-- Candidate

-- |A Candidate represents a potential match going into the ranker
data Candidate = Candidate
    ResolvedToken -- ^ The actual resolved token we are considering
    Double -- ^ naive Bayes log-likelihood - sum of LL of all rules used
    Bool -- ^ Does the ResolvedToken's dimension match the caller's request?
  deriving (Eq, Show)

instance Ord Candidate where
  compare (Candidate Resolved{range = Range s1 e1, node = Node{token = Token d1 _}} score1 t1)
          (Candidate Resolved{range = Range s2 e2, node = Node{token = tok2}} score2 t2)
    | isDimension d1 tok2 = case starts of
        EQ -> case ends of
          EQ -> compare score1 score2
          z -> z
        LT -> case ends of
          LT -> EQ
          _ -> GT
        GT -> case ends of
          GT -> EQ
          _ -> LT
    | t1 == t2 = compRange
    | t1 && compRange == GT = GT
    | t2 && compRange == LT = LT
    | otherwise = EQ
      where
        starts = compare s1 s2
        ends = compare e1 e2
        -- a > b if a recovers b
        compRange = case starts of
          EQ -> ends
          LT -> case ends of
            LT -> EQ
            _  -> GT
          GT -> case ends of
            GT -> EQ
            _  -> LT
