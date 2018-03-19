-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Duckling.Types where

import Control.DeepSeq
import Data.Aeson
import Data.GADT.Compare
import Data.Hashable
import Data.Maybe
import Data.String
import Data.Text (Text)
import Data.Typeable ((:~:)(Refl), Typeable)
import GHC.Generics
import Prelude
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as Text
import qualified Text.Regex.Base as R
import qualified Text.Regex.PCRE as PCRE

import Duckling.Dimensions.Types
import Duckling.Resolve

-- -----------------------------------------------------------------
-- Token

data Token = forall a . (Resolve a, Eq a, Hashable a, Show a, NFData a) =>
  Token (Dimension a) a

deriving instance Show Token
instance Eq Token where
  Token d1 v1 == Token d2 v2 = case geq d1 d2 of
    Just Refl -> v1 == v2
    Nothing   -> False

instance Hashable Token where
  hashWithSalt s (Token dim v) = hashWithSalt s (dim, v)

instance NFData Token where
  rnf (Token _ v) = rnf v

isDimension :: Dimension a -> Token -> Bool
isDimension dim (Token dim' _) = isJust $ geq dim dim'

data Node = Node
  { nodeRange :: Range
  , token     :: Token
  , children  :: [Node]
  , rule      :: Maybe Text
  } deriving (Eq, Generic, Hashable, Show, NFData)

data ResolvedToken = Resolved
  { range :: Range
  , node :: Node
  , jsonValue :: Value
  , isLatent :: Bool
  } deriving (Eq, Show)

instance Ord ResolvedToken where
  compare (Resolved range1 _ json1 latent1) (Resolved range2 _ json2 latent2) =
    case compare range1 range2 of
      EQ -> case compare (toJText json1) (toJText json2) of
        EQ -> compare latent1 latent2
        z -> z
      z  -> z

data Candidate = Candidate ResolvedToken Double Bool
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

data Range = Range Int Int
  deriving (Eq, Ord, Generic, Hashable, Show, NFData)

type Production = [Token] -> Maybe Token
type Predicate = Token -> Bool
data PatternItem = Regex PCRE.Regex | Predicate Predicate

type Pattern = [PatternItem]

data Rule = Rule
  { name :: Text
  , pattern :: Pattern
  , prod :: Production
  }

instance Show Rule where
  show (Rule name _ _) = show name

data Entity = Entity
  { dim    :: Text
  , body   :: Text
  , value  :: Value
  , start  :: Int
  , end    :: Int
  , latent :: Bool
  , enode  :: Node
  } deriving (Eq, Generic, Show, NFData)

instance ToJSON Entity where
  toJSON ent = object
    [ "dim"    .= dim ent
    , "body"   .= body ent
    , "value"  .= value ent
    , "start"  .= start ent
    , "end"    .= end ent
    , "latent" .= latent ent
    ]

toJText :: ToJSON x => x -> Text
toJText = Text.decodeUtf8 . LB.toStrict . encode

-- -----------------------------------------------------------------
-- Predicates helpers

regex :: String -> PatternItem
regex = Regex . R.makeRegexOpts compOpts execOpts
  where
    compOpts = PCRE.defaultCompOpt + PCRE.compCaseless + PCRE.compUTF8
    execOpts = PCRE.defaultExecOpt

dimension :: Typeable a => Dimension a -> PatternItem
dimension value = Predicate $ isDimension value
