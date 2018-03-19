-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Locale
  ( Lang(..)
  , Locale(..)
  , Region(..)
  , allLocales
  , makeLocale
  ) where

import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import GHC.Generics
import Prelude
import TextShow (TextShow)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified TextShow as TS

-- | ISO 639-1 Language.
-- See https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
data Lang
  = AR
  | BG
  | CS
  | DA
  | DE
  | EL
  | EN
  | ES
  | ET
  | FR
  | GA
  | HE
  | HI
  | HR
  | HU
  | ID
  | IT
  | JA
  | KA
  | KO
  | MY
  | NB
  | NE
  | NL
  | PL
  | PT
  | RO
  | RU
  | SV
  | TA
  | TR
  | UK
  | VI
  | ZH
  deriving (Bounded, Enum, Eq, Generic, Hashable, Ord, Read, Show)

instance TextShow Lang where
  showb = TS.fromString . show

-- | ISO 3166-1 alpha-2 Country code (includes regions and territories).
-- See https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2
data Region
  = AU
  | BZ
  | CA
  | CN
  | GB
  | HK
  | IE
  | IN
  | JM
  | MO
  | NZ
  | PH
  | TT
  | TW
  | US
  | ZA
  deriving (Bounded, Enum, Eq, Generic, Hashable, Ord, Read, Show)

instance TextShow Region where
  showb = TS.fromString . show

data Locale = Locale Lang (Maybe Region)
  deriving (Eq, Generic, Hashable, Ord)

instance Show Locale where
  show (Locale lang Nothing) = show lang ++ "_XX"
  show (Locale lang (Just region)) = show lang ++ "_" ++ show region

instance TextShow Locale where
  showb = TS.fromString . show

makeLocale :: Lang -> Maybe Region -> Locale
makeLocale lang Nothing = Locale lang Nothing
makeLocale lang (Just region)
  | HashSet.member region locales = Locale lang $ Just region
  | otherwise = Locale lang Nothing
  where
    locales = HashMap.lookupDefault HashSet.empty lang allLocales

allLocales :: HashMap Lang (HashSet Region)
allLocales = HashMap.fromList
  [ (EN, HashSet.fromList [AU, BZ, CA, GB, IN, IE, JM, NZ, PH, ZA, TT, US])
  , (ZH, HashSet.fromList [CN, HK, MO, TW])
  ]
