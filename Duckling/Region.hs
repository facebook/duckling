-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Region
  ( Region(..)
  ) where

import Data.Hashable
import GHC.Generics
import Prelude
import TextShow (TextShow)
import qualified TextShow as TS

-- | ISO 3166-1 alpha-2 Country code (includes regions and territories).
-- See https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2
data Region
  = AU
  | BE
  | BZ
  | CA
  | CN
  | GB
  | HK
  | IE
  | IN
  | JM
  | MN
  | MO
  | NL
  | NZ
  | PH
  | TT
  | TW
  | US
  | ZA
  deriving (Bounded, Enum, Eq, Generic, Hashable, Ord, Read, Show)

instance TextShow Region where
  showb = TS.fromString . show
