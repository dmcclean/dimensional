{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Numeric.Units.Dimensional.Presentation.Units
(
  -- * Presentation Units
  PresentationUnit(..)
  -- * Construction
, simpleUnit
, compositeUnit
, compositeUnit'
, prefixedUnit
, siPrefixedUnit
, majorSiPrefixedUnit
  -- Querying
, fixUnit
, units
)
where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.UnitNames (PrefixSet, siPrefixes, majorSiPrefixes)
import Data.ExactPi (ExactPi)
import Data.List.NonEmpty (NonEmpty(..))

data PresentationUnit d = PresentationUnit (NonEmpty (Unit 'NonMetric d ExactPi))
                        | PrefixedUnit PrefixSet (Unit 'Metric d ExactPi)

fixUnit :: (RealFrac a, Floating a) => PresentationUnit d -> Quantity d a -> PresentationUnit d
fixUnit (PrefixedUnit ps u) q = simpleUnit . exactify $ withAppropriatePrefix' ps (changeRepApproximate u) q
fixUnit u _ = u

units :: PresentationUnit d -> NonEmpty (Unit 'NonMetric d ExactPi)
units (PrefixedUnit _ u) = weaken u :| []
units (PresentationUnit us) = us

compositeUnit :: NonEmpty (Unit 'NonMetric d ExactPi) -> PresentationUnit d
compositeUnit = PresentationUnit

compositeUnit' :: (KnownDimension d) => [Unit 'NonMetric d ExactPi] -> PresentationUnit d
compositeUnit' [] = simpleUnit siUnit
compositeUnit' (u:us) = compositeUnit (u :| us)

simpleUnit :: Unit m d ExactPi -> PresentationUnit d
simpleUnit = PresentationUnit . (:| []) . weaken

prefixedUnit :: PrefixSet -> Unit 'Metric d ExactPi -> PresentationUnit d
prefixedUnit = PrefixedUnit

siPrefixedUnit :: Unit 'Metric d ExactPi -> PresentationUnit d
siPrefixedUnit = prefixedUnit siPrefixes

majorSiPrefixedUnit :: Unit 'Metric d ExactPi -> PresentationUnit d
majorSiPrefixedUnit = prefixedUnit majorSiPrefixes
                        