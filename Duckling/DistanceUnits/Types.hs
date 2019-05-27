-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


-- | This module shadows names of some units previously defined in
-- 'Duckling.Distance.Types'.
-- Therefore, the units from that module must be used qualified by module.
module Duckling.DistanceUnits.Types
  ( ContextualDistance (..)
  , toSystemUnit
  , toRawUnit
  ) where

import Data.Semigroup
import Data.Tuple.Extra (both)
import Prelude

import qualified Duckling.Distance.Types as TDist


-- | Supports deferred resolution of ambiguous units.
-- Note that this sum type cannot be simply replaced by
-- "Maybe (ContextualDistance Double DeferrableUnit)"
-- (with "Nothing" representing "Nonrelatable").
-- See "NOTE A" below.
data ContextualDistance
  = Nonrelatable
    -- ^ If two different ambiguous units were to be composed, then
    -- it would not be possible to decide which to resolve to. As of today,
    -- this won't happen since for now "M" is the lone ambiguous unit.
  | ContextualDistance Double DeferrableUnit

-- | This represents Units that have not yet been established as unrelatable.
-- The "Definite" constructor is purposely listed in front of "Ambiguous",
-- so that it will be preferred when taking choosing the "min" between
-- an "Ambiguous" and "Definite" unit.
data DeferrableUnit
  = Definite SystemUnit
  | Ambiguous AmbiguousUnit
  deriving (Eq, Ord)

-- | These measurement-system-specific units exist to disambiguate
-- the \"M\" of the 'Unit' type and to maintain the measurement-system context
-- for resolving to the appropriate precision.
--
-- It's also just handy to have separate programmatically accessible lists
-- of units for each type of measurement system.
data SystemUnit
  = Metric MetricUnit
  | Imperial ImperialUnit
  deriving (Eq, Ord)

data ImperialUnit
  = Inch
  | Foot
  | Yard
  | Mile
  deriving (Eq, Ord)

data MetricUnit
  = Millimetre
  | Centimetre
  | Metre
  | Kilometre
  deriving (Eq, Ord)

-- | Currently there is only one actually ambiguous unit, but this design allows
-- for expansion.
data AmbiguousUnit
  = M -- ^ "Miles" or "Metres"
  deriving (Eq, Ord)

-- | Represents a value with an unambiguous unit
data UnitValuePair = UnitValuePair SystemUnit Double

sumScaledUnits :: SystemUnit -> (UnitValuePair, UnitValuePair) -> Double
sumScaledUnits preferredUnit = uncurry (+) . both (scaleUnits preferredUnit)

-- | Determine the definite meaning of an "Ambiguous" unit using the context of
-- another "Definite" unit
reconcileAmbiguousWithDefinite ::
     Double
  -> AmbiguousUnit
  -> Double
  -> SystemUnit
  -> ContextualDistance
reconcileAmbiguousWithDefinite av au dv du =
  ContextualDistance combinedValue $ Definite preferredUnit
  where
    resolvedAmbiguousUnit = resolveUnit au du
    preferredUnit = du `min` resolvedAmbiguousUnit
    combinedValue = sumScaledUnits preferredUnit
      (UnitValuePair resolvedAmbiguousUnit av, UnitValuePair du dv)

-- | When both Metric and Imperial units are given, resolve to Metric.
-- Otherwise, preserve the original measurement system and use the smaller unit.
-- For the purpose of this resolution, all Metric units are considered "smaller"
-- than Imperial units.
instance Semigroup ContextualDistance where

  _ <> Nonrelatable = Nonrelatable
  Nonrelatable <> _ = Nonrelatable

  (ContextualDistance v1 u@(Ambiguous u1))
    <> (ContextualDistance v2 (Ambiguous u2))
      | u1 == u2  = ContextualDistance (v1 + v2) u
      | otherwise = Nonrelatable
      -- NOTE A: Needing to return "Nonrelatable" in this edge case is why
      -- the two-member "ContextualDistance" sum type cannot be simply
      -- represented as (Maybe (ContextualDistance Double DeferrableUnit)).

  (ContextualDistance av (Ambiguous au))
    <> (ContextualDistance dv (Definite du)) =
      reconcileAmbiguousWithDefinite av au dv du

  (ContextualDistance dv (Definite du))
    <> (ContextualDistance av (Ambiguous au)) =
      reconcileAmbiguousWithDefinite av au dv du

  (ContextualDistance v1 (Definite u1))
    <> (ContextualDistance v2 (Definite u2)) =
      ContextualDistance v $ Definite u
      where
        v = sumScaledUnits u (UnitValuePair u1 v1, UnitValuePair u2 v2)
        u = u1 `min` u2

resolveUnit :: AmbiguousUnit -> SystemUnit -> SystemUnit
resolveUnit M (Metric _) = Metric Metre
resolveUnit M (Imperial _) = Imperial Mile


-- | Disambiguation of original Unit type
toSystemUnit :: TDist.Unit -> DeferrableUnit
toSystemUnit TDist.M          = Ambiguous M
toSystemUnit TDist.Millimetre = Definite $ Metric Millimetre
toSystemUnit TDist.Centimetre = Definite $ Metric Centimetre
toSystemUnit TDist.Metre      = Definite $ Metric Metre
toSystemUnit TDist.Kilometre  = Definite $ Metric Kilometre
toSystemUnit TDist.Inch       = Definite $ Imperial Inch
toSystemUnit TDist.Foot       = Definite $ Imperial Foot
toSystemUnit TDist.Yard       = Definite $ Imperial Yard
toSystemUnit TDist.Mile       = Definite $ Imperial Mile

-- | Reconversion to original Unit type
toRawUnit :: DeferrableUnit -> TDist.Unit
toRawUnit (Ambiguous M) = TDist.M
toRawUnit (Definite (Metric Millimetre)) = TDist.Millimetre
toRawUnit (Definite (Metric Centimetre)) = TDist.Centimetre
toRawUnit (Definite (Metric Metre)) = TDist.Metre
toRawUnit (Definite (Metric Kilometre)) = TDist.Kilometre
toRawUnit (Definite (Imperial Inch)) = TDist.Inch
toRawUnit (Definite (Imperial Foot)) = TDist.Foot
toRawUnit (Definite (Imperial Yard)) = TDist.Yard
toRawUnit (Definite (Imperial Mile)) = TDist.Mile

-- | Convert a distance to the given units.
-- This only works if the unit is unambiguous.
scaleUnits ::
     SystemUnit -- ^ target unit
  -> UnitValuePair -- ^ Original unit and value
  -> Double
scaleUnits targetUnit (UnitValuePair startingUnit v)
  | startingUnit == targetUnit = v
  | otherwise = inSIUnits v startingUnit / inSIUnits 1 targetUnit

-- | This is used when distances
-- must be normalized across measurement systems.
-- The Metric metre is the Standard International unit of distance.
inSIUnits :: Double -> SystemUnit -> Double
inSIUnits val (Metric u) = inMetres u val
inSIUnits val (Imperial u) = inInches u val * metersPerInch

-- | This conversion factor is exact.
metersPerInch :: Double
metersPerInch = 0.0254

inMetres :: Fractional a => MetricUnit -> a -> a
inMetres Millimetre n = n / 1000
inMetres Centimetre n = n / 100
inMetres Metre      n = n
inMetres Kilometre  n = n * 1000

inInches :: Num a => ImperialUnit -> a -> a
inInches Inch n = n
inInches Foot n = 12   * n
inInches Yard n = 3    * inInches Foot n
inInches Mile n = 5280 * inInches Foot n
