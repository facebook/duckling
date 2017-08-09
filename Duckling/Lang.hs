-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Lang
  ( Lang(..)
  ) where

import Prelude

import Data.Hashable
import GHC.Generics
import TextShow (TextShow)
import qualified TextShow as TS

data Lang
  = AR
  | BG
  | CS
  | DA
  | DE
  | EN
  | ES
  | ET
  | FR
  | GA
  | HE
  | HR
  | HU
  | ID
  | IT
  | JA
  | KO
  | MY
  | NB
  | NL
  | PL
  | PT
  | RO
  | RU
  | SV
  | TR
  | UK
  | VI
  | ZH
  deriving (Bounded, Enum, Eq, Generic, Hashable, Ord, Read, Show)

instance TextShow Lang where
  showb = TS.fromString . show
