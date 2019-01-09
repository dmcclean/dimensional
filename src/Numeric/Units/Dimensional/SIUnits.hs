{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

{- |
   Copyright  : Copyright (C) 2006-2018 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn@buckwalter.se
   Stability  : Stable
   Portability: GHC only

= Summary

This module defines the SI prefixes, the SI base units and the SI
derived units. It also defines the units outside of the SI that are
accepted for use with the SI. Any chapters, sections or tables
referenced are from <#note1 [1]> unless otherwise specified.

= References

1. #note1# http://physics.nist.gov/Pubs/SP811/
2. #note2# http://en.wikipedia.org/wiki/Minute_of_arc
3. #note3# http://en.wikipedia.org/wiki/Astronomical_unit

-}

module Numeric.Units.Dimensional.SIUnits
(
  -- * SI Base Units
  -- $base-units
  metre, meter, gram, second, ampere, kelvin, mole, candela,
  -- * SI Derived Units
  -- $derived-units
  radian, steradian, hertz, newton, pascal, joule, watt, coulomb, volt, farad, ohm, siemens, weber, tesla, henry, lumen, lux,
  -- ** Celsius Temperature
  -- $celsius
  degreeCelsius, fromDegreeCelsiusAbsolute, toDegreeCelsiusAbsolute,
  -- ** Units Admitted for Reasons of Safeguarding Human Health
  -- $health
  becquerel, gray, sievert, katal,
  -- * Units Accepted for Use with the SI
  -- $accepted-units
  minute, hour, day,
  hectare, litre, liter, tonne, metricTon,
  -- ** Units of Plane Angle
  -- $arc-units
  degree, arcminute, arcsecond,
  -- $arc-units-alternate
  degreeOfArc, minuteOfArc, secondOfArc,
  -- ** Units Formerly Defined By Experiment
  -- $values-obtained-experimentally
  astronomicalUnit,
  -- * SI Prefixes
  -- $multiples
  deka, deca, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta,
  -- $submultiples
  deci, centi, milli, micro, nano, pico, femto, atto, zepto, yocto,
  -- $reified-prefixes
  Prefix, applyPrefix, applyOptionalPrefix, siPrefixes, appropriatePrefix, withAppropriatePrefix, appropriatePrefix', withAppropriatePrefix'
)
where

import Data.List (sortBy, find)
import Data.Maybe (maybe)
import Data.Ord (comparing, Down(..))
import Data.Ratio
import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.Quantities
import Numeric.Units.Dimensional.UnitNames (Prefix, siPrefixes, scaleExponent)
import qualified Numeric.Units.Dimensional.UnitNames as N
import Numeric.Units.Dimensional.UnitNames.Internal (metricAtomic, atomic)
import Numeric.Units.Dimensional.UnitNames.Languages (siunitx, usEnglish, ucum)
import qualified Numeric.Units.Dimensional.UnitNames.Internal as I
import qualified Numeric.Units.Dimensional.UnitNames.Prefixes as Prefix
import Numeric.NumType.DK.Integers ( pos3 )
import Prelude ( Eq(..), ($), (.), Num, Fractional, Floating, RealFrac(..), Maybe(..), otherwise, error, Ord(..), fst, snd, Int, Bool, fmap, mod, (&&))
import qualified Prelude

{- $multiples
Prefixes are used to form decimal multiples and submultiples of SI
Units as described in section 4.4. We will define the SI prefixes
in terms of the 'prefix' function which applies a scale factor to a
unit.

By defining SI prefixes as functions applied to a 'Unit' we satisfy
section 6.2.6 "Unacceptability of stand-alone prefixes".

We define all SI prefixes from Table 5. Multiples first.
-}

applyMultiple :: (Num a) => Prefix -> Unit 'Metric d a -> Unit 'NonMetric d a
applyMultiple p u | denominator x == 1 = mkUnitZ n' (numerator x) u
                  | otherwise = error "Attempt to apply a submultiple prefix as a multiple."
  where
    n' = N.applyPrefix p (name u)
    x = N.scaleFactor p

deka, deca, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta
  :: Num a => Unit 'Metric d a -> Unit 'NonMetric d a
deka  = applyMultiple Prefix.deka -- International English.
deca  = deka      -- American English.
hecto = applyMultiple Prefix.hecto
kilo  = applyMultiple Prefix.kilo
mega  = applyMultiple Prefix.mega
giga  = applyMultiple Prefix.giga
tera  = applyMultiple Prefix.tera
peta  = applyMultiple Prefix.peta
exa   = applyMultiple Prefix.exa
zetta = applyMultiple Prefix.zetta
yotta = applyMultiple Prefix.yotta

{- $submultiples
Then the submultiples.
-}

-- | Applies a 'Prefix' to a 'Metric' 'Unit', creating a 'NonMetric' unit.
applyPrefix :: (Fractional a) => Prefix -> Unit 'Metric d a -> Unit 'NonMetric d a
applyPrefix p u = mkUnitQ n' x u
  where
    n' = N.applyPrefix p (name u)
    x = N.scaleFactor p

-- | Applies an optional 'Prefix' to a 'Metric' 'Unit', creating a 'NonMetric' unit.
applyOptionalPrefix :: (Fractional a) => Maybe Prefix -> Unit 'Metric d a -> Unit 'NonMetric d a
applyOptionalPrefix Nothing = weaken
applyOptionalPrefix (Just p) = applyPrefix p

deci, centi, milli, micro, nano, pico, femto, atto, zepto, yocto
  :: Fractional a => Unit 'Metric d a -> Unit 'NonMetric d a
deci  = applyPrefix Prefix.deci
centi = applyPrefix Prefix.centi
milli = applyPrefix Prefix.milli
micro = applyPrefix Prefix.micro
nano  = applyPrefix Prefix.nano
pico  = applyPrefix Prefix.pico
femto = applyPrefix Prefix.femto
atto  = applyPrefix Prefix.atto
zepto = applyPrefix Prefix.zepto
yocto = applyPrefix Prefix.yocto

{- $reified-prefixes

We supply an explicit representation of an SI prefix, along with a function to apply one and a
list of all prefixes defined by the SI.

-}

-- | Selects the appropriate 'Prefix' to use with a 'Metric' unit when using it to display
-- a particular 'Quantity', or 'Nothing' if the supplied unit should be used without a prefix.
--
-- The appropriate prefix is defined to be the largest prefix such that the resulting value
-- of the quantity, expressed in the prefixed unit, is greater than or equal to one.
--
-- Note that the supplied prefix need not be 'Metric'. This is intended for use to compute a prefix to insert
-- somewhere in the denominator of a composite (and hence 'NonMetric') unit.
appropriatePrefix :: (Floating a, RealFrac a) => Unit m d a -> Quantity d a -> Maybe Prefix
appropriatePrefix u q = selectPrefix (<= e)
  where
    val = q /~ u
    e = Prelude.floor $ Prelude.logBase 10 val :: Prelude.Int

-- | Selects the appropriate 'Prefix' to use with a 'Metric' unit when using it to display
-- a particular 'Quantity', or 'Nothing' if the supplied unit should be used without a prefix.
--
-- The appropriate prefix is defined to be the largest prefix such that the resulting value
-- of the quantity, expressed in the prefixed unit, is greater than or equal to one. Only those prefixes
-- whose 'scaleExponent' is a multiple of @3@ are considered.
--
-- Note that the supplied prefix need not be 'Metric'. This is intended for use to compute a prefix to insert
-- somewhere in the denominator of a composite (and hence 'NonMetric') unit.
appropriatePrefix' :: (Floating a, RealFrac a) => Unit m d a -> Quantity d a -> Maybe Prefix
appropriatePrefix' u q = selectPrefix (\x -> x `mod` 3 == 0 && x <= e)
  where
    val = q /~ u
    e = Prelude.floor $ Prelude.logBase 10 val :: Prelude.Int

-- Selects the first prefix in the list of prefix candidates whose scale exponent matches the supplied predicate.
selectPrefix :: (Int -> Bool) -> Maybe Prefix
selectPrefix p = maybe (Just . Prelude.head $ siPrefixes) snd $ find (p . fst) prefixCandidates

-- This is a list of candidate prefixes and the least scale exponent at which each applies.
prefixCandidates :: [(Int, Maybe Prefix)]
prefixCandidates = sortBy (comparing $ Down . fst) $ (0, Nothing) : fmap (\x -> (scaleExponent x, Just x)) siPrefixes

-- | Constructs a version of a 'Metric' unit, by possibly applying a 'Prefix' to it, appropriate
-- for display of a particular 'Quantity'.
--
-- The appropriate prefix is defined to be the largest prefix such that the resulting value
-- of the quantity, expressed in the prefixed unit, is greater than or equal to one.
withAppropriatePrefix :: (Floating a, RealFrac a) => Unit 'Metric d a -> Quantity d a -> Unit 'NonMetric d a
withAppropriatePrefix u q = applyOptionalPrefix (appropriatePrefix u q) u

-- | Constructs a version of a 'Metric' unit, by possibly applying a 'Prefix' to it, appropriate
-- for display of a particular 'Quantity'.
--
-- The appropriate prefix is defined to be the largest prefix such that the resulting value
-- of the quantity, expressed in the prefixed unit, is greater than or equal to one. Only those prefixes
-- whose 'scaleExponent' is a multiple of @3@ are considered.
withAppropriatePrefix' :: (Floating a, RealFrac a) => Unit 'Metric d a -> Quantity d a -> Unit 'NonMetric d a
withAppropriatePrefix' u q = applyOptionalPrefix (appropriatePrefix' u q) u

{- $base-units
These are the base units from section 4.1. To avoid a
myriad of one-letter functions that would doubtlessly cause clashes
and frustration in users' code we spell out all unit names in full,
as we did for prefixes. We also elect to spell the unit names in
singular form, as allowed by section 9.7 "Other spelling conventions".

We define the SI base units in the order of table 1.
-}

metre, meter :: Num a => Unit 'Metric DLength a
metre = mkUnitZ I.nMeter 1 siUnit -- International English.
meter = metre         -- American English.

{-

For mass the SI base unit is kilogram. For sensible prefixes we
define gram here (see section 6.2.7 "Prefixes and the kilogram").
The drawback is that we are forced to use 'Fractional'.

-}

gram    :: Fractional a => Unit 'Metric DMass a
gram    = mkUnitQ I.nGram 1e-3 siUnit

second  :: Num a => Unit 'Metric DTime a
second  = mkUnitZ I.nSecond 1 siUnit

ampere  :: Num a => Unit 'Metric DElectricCurrent a
ampere  = mkUnitZ I.nAmpere 1 siUnit

kelvin  :: Num a => Unit 'Metric DThermodynamicTemperature a
kelvin  = mkUnitZ I.nKelvin 1 siUnit

mole    :: Num a => Unit 'Metric DAmountOfSubstance a
mole    = mkUnitZ I.nMole 1 siUnit

candela :: Num a => Unit 'Metric DLuminousIntensity a
candela = mkUnitZ I.nCandela 1 siUnit

{- $derived-units
From Table 3, SI derived units with special names and symbols, including the
radian and steradian.
-}

radian :: Num a => Unit 'Metric DPlaneAngle a
radian = mkUnitZ n 1 siUnit -- meter * meter ^ neg1
  where
    n = metricAtomic "rad" "rad" "radian" [(siunitx, "\\radian")]

steradian :: Num a => Unit 'Metric DSolidAngle a
steradian = mkUnitZ n 1 siUnit -- meter ^ pos2 * meter ^ neg2
  where
    n = metricAtomic "sr" "sr" "steradian" [(siunitx, "\\steradian")]

hertz :: Num a => Unit 'Metric DFrequency a
hertz = mkUnitZ n 1 siUnit
  where
    n = metricAtomic "Hz" "Hz" "Hertz" [(siunitx, "\\hertz")]

newton :: Num a => Unit 'Metric DForce a
newton = mkUnitZ n 1 siUnit
  where
    n = metricAtomic "N" "N" "Newton" [(siunitx, "\\newton")]

pascal :: Num a => Unit 'Metric DPressure a
pascal = mkUnitZ n 1 siUnit
  where
    n = metricAtomic "Pa" "Pa" "Pascal" [(siunitx, "\\pascal")]

joule :: Num a => Unit 'Metric DEnergy a
joule = mkUnitZ n 1 siUnit
  where
    n = metricAtomic "J" "J" "Joule" [(siunitx, "\\joule")]

watt :: Num a => Unit 'Metric DPower a
watt = mkUnitZ n 1 siUnit
  where
    n = metricAtomic "W" "W" "Watt" [(siunitx, "\\watt")]

coulomb :: Num a => Unit 'Metric DElectricCharge a
coulomb = mkUnitZ n 1 siUnit
  where
    n = metricAtomic "C" "C" "Coulomb" [(siunitx, "\\coloumb")]

volt :: Num a => Unit 'Metric DElectricPotential a
volt = mkUnitZ n 1 siUnit
  where
    n = metricAtomic "V" "V" "Volt" [(siunitx, "\\volt")]

farad :: Num a => Unit 'Metric DCapacitance a
farad = mkUnitZ n 1 siUnit
  where
    n = metricAtomic "F" "F" "Farad" [(siunitx, "\\farad")]

ohm :: Num a => Unit 'Metric DElectricResistance a
ohm = mkUnitZ n 1 siUnit
  where
    n = metricAtomic "Ohm" "Ω" "Ohm" [(siunitx, "\\ohm")]

siemens :: Num a => Unit 'Metric DElectricConductance a
siemens = mkUnitZ n 1 siUnit
  where
    n = metricAtomic "S" "S" "Siemens" [(siunitx, "\\siemens")]

weber :: Num a => Unit 'Metric DMagneticFlux a
weber = mkUnitZ n 1 siUnit
  where
    n = metricAtomic "Wb" "Wb" "Weber" [(siunitx, "\\weber")]

tesla :: Num a => Unit 'Metric DMagneticFluxDensity a
tesla = mkUnitZ n 1 siUnit
  where
    n = metricAtomic "T" "T" "Tesla" [(siunitx, "\\tesla")]

henry :: Num a => Unit 'Metric DInductance a
henry = mkUnitZ n 1 siUnit
  where
    n = metricAtomic "H" "H" "Henry" [(siunitx, "\\henry")]

{-
We defer the definition of Celcius temperature to another section (would
appear here if we stricly followed table 3).
-}

lumen :: Num a => Unit 'Metric DLuminousFlux a
lumen = mkUnitZ n 1 siUnit
  where
    n = metricAtomic "lm" "lm" "lumen" [(siunitx, "\\lumen")]

lux :: Num a => Unit 'Metric DIlluminance a
lux = mkUnitZ n 1 siUnit
  where
    n = metricAtomic "lx" "lx" "lux" [(siunitx, "\\lux")]

{- $celsius
A problematic area is units which increase proportionally to the
base SI units but cross zero at a different point. An example would
be degrees Celsius (see section 4.2.1.1). The author feels that it
is appropriate to define a unit for use with relative quantities
(taking only into account the proportionality) and complement the
unit with functions for converting absolute values.

The function 'fromDegreeCelsiusAbsolute' should be used in lieu of
"*~ degreeCelsius" when working with absolute temperatures. Similarily,
'toDegreeCelsiusAbsolute' should be used in lieu of "/~ degreeCelsius"
when working with absolute temperatures.
-}

degreeCelsius :: Num a => Unit 'Metric DCelsiusTemperature a
degreeCelsius = kelvin

fromDegreeCelsiusAbsolute :: Floating a => a -> ThermodynamicTemperature a
fromDegreeCelsiusAbsolute x = x *~ degreeCelsius + 273.15 *~ degreeCelsius
toDegreeCelsiusAbsolute :: Floating a => ThermodynamicTemperature a -> a
toDegreeCelsiusAbsolute x = (x - 273.15 *~ degreeCelsius) /~ degreeCelsius

{- $health

The last units from Table 3 are SI derived units with special names and symbols admitted for reasons
of safeguarding human health.
-}

becquerel :: Num a => Unit 'Metric DActivity a
becquerel = mkUnitZ n 1 siUnit
  where
    n = metricAtomic "Bq" "Bq" "Becquerel" [(siunitx, "\\becquerel")]

gray :: Num a => Unit 'Metric DAbsorbedDose a
gray = mkUnitZ n 1 siUnit
  where
    n = metricAtomic "Gy" "Gy" "Gray" [(siunitx, "\\gray")]

sievert :: Num a => Unit 'Metric DDoseEquivalent a
sievert = mkUnitZ n 1 siUnit
  where
    n = metricAtomic "Sv" "Sv" "Sievert" [(siunitx, "\\sievert")]

katal :: Num a => Unit 'Metric DCatalyticActivity a
katal = mkUnitZ n 1 siUnit
  where
    n = metricAtomic "kat" "kat" "katal" [(siunitx, "\\katal")]

{- $accepted-units
There are several units that are not strictly part of the SI but
are either permanently or temporarily accepted for use with the SI.
We define the permanently accepted ones in this module.

From Table 6, Units accepted for use with the SI.

We start with time which we grant exclusive rights to 'minute' and
'second'.
-}
minute, hour, day :: Num a => Unit 'NonMetric DTime a
minute = mkUnitZ n 60 second
  where
    n = atomic "min" "minute" [(ucum, "min"),
                               (siunitx, "\\minute")]

hour = mkUnitZ n 60 minute
  where
    n = atomic "h" "hour" [(ucum, "h"),
                           (siunitx, "\\hour")]

day = mkUnitZ n 24 hour -- Mean solar day.
  where
    n = atomic "d" "day" [(ucum, "d"),
                          (siunitx, "\\day")]

{- $arc-units

Since 'minute' and 'second' are already in use for time we use
'arcminute' and 'arcsecond' <#note2 [2]> for plane angle instead.
-}

degree, arcminute, arcsecond :: Floating a => Unit 'NonMetric DPlaneAngle a
degree = mkUnitR n (Prelude.pi Prelude./ 180) radian
  where
    n = atomic "°" "degree" [(ucum, "deg"),
                             (siunitx, "\\degree")]

arcminute = mkUnitR n (Prelude.recip 60) degreeOfArc
  where
    n = atomic "'" "arcminute" [(ucum, "'"),
                                (siunitx, "\\arcminute")]

arcsecond = mkUnitR n (Prelude.recip 60) minuteOfArc
  where
    n = atomic "''" "arcsecond" [(ucum, "''"),
                                 (siunitx, "\\arcsecond")]

{- $arc-units-alternate
Alternate (longer) forms of the above. In particular 'degreeOfArc'
can be used if there is a percieved need to disambiguate from e.g.
temperature.
-}

degreeOfArc, minuteOfArc, secondOfArc :: Floating a => Unit 'NonMetric DPlaneAngle a
degreeOfArc = degree
secondOfArc = arcsecond
minuteOfArc = arcminute

hectare :: Fractional a => Unit 'NonMetric DArea a
hectare = mkUnitQ n 1 $ square (hecto meter)
  where
    n = atomic "ha" "hectare" [(siunitx, "\\hectare")]

litre, liter :: Fractional a => Unit 'Metric DVolume a
litre = mkUnitQ n 1 $ deci meter ^ pos3 -- International English.
  where
    n = metricAtomic "L" "L" "litre" [(siunitx, "\\litre"),
                                      (usEnglish, "liter")]
liter = litre             -- American English.

tonne, metricTon :: Num a => Unit 'Metric DMass a
tonne     = mkUnitZ n 1000 siUnit -- Name in original SI text.
  where
    n = metricAtomic "t" "t" "tonne" [(siunitx, "\\tonne"),
                                      (usEnglish, "metric ton")]
metricTon = tonne                   -- American name.

{- $values-obtained-experimentally
We decline to provide here those units - listed in Table 7 - which,
while accepted for use with the SI, have values which are determined experimentally.
For versioning purposes, those units can be found in "Numeric.Units.Dimensional.NonSI".

However, in 2012 the IAU redefined the astronomical unit as a conventional
unit of length directly tied to the meter, with a length of exactly
149 597 870 700 m and the official abbreviation of au <#note3 [3]>. We therefore include it here.
-}

astronomicalUnit :: Num a => Unit 'NonMetric DLength a
astronomicalUnit = mkUnitZ n 149597870700 meter
  where
    n = atomic "AU" "astronomical unit" [(ucum, "AU"),
                                         (siunitx, "\\astronomicalunit")]
