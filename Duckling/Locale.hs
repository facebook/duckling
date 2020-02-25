-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoRebindableSyntax #-}
module Duckling.Locale
  ( Lang(..)
  , Locale(..)
  , Region
      ( AU
      , BE
      , BZ
      , CA
      , CL
      , CN
      , CO
      , GB
      , HK
      , IE
      , IN
      , JM
      , MO
      , MX
      , NZ
      , PE
      , PH
      , TT
      , TW
      , US
      , VE
      , ZA
      )
  , allLocales
  , makeLocale
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import GHC.Generics
import Prelude
import TextShow (TextShow)
import qualified TextShow as TS

import Duckling.Region hiding
  ( AR
  , ES
  , NL
  )
import qualified Duckling.Region as R
  ( Region
      ( AR
      , ES
      , NL
      )
  )

-- | ISO 639-1 Language.
-- See https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
data Lang
  = AF
  | AR
  | BG
  | BN
  | CS
  | DA
  | DE
  | EL
  | EN
  | ES
  | ET
  | FI
  | FR
  | GA
  | HE
  | HI
  | HR
  | HU
  | ID
  | IS
  | IT
  | JA
  | KA
  | KN
  | KM
  | KO
  | LO
  | ML
  | MN
  | MY
  | NB
  | NE
  | NL
  | PL
  | PT
  | RO
  | RU
  | SK
  | SV
  | SW
  | TA
  | TH
  | TR
  | UK
  | VI
  | ZH
  deriving (Bounded, Enum, Eq, Generic, Hashable, Ord, Read, Show)

instance TextShow Lang where
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
allLocales =
  HashMap.fromList
    [ (EN, HashSet.fromList [AU, BZ, CA, GB, IN, IE, JM, NZ, PH, ZA, TT, US])
    , (ES, HashSet.fromList [R.AR, CL, CO, R.ES, MX, PE, VE])
    , (NL, HashSet.fromList [BE, R.NL])
    , (ZH, HashSet.fromList [CN, HK, MO, TW])
    ]
