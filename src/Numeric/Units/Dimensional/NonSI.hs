{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumDecimals #-}

{- |
   Copyright  : Copyright (C) 2006-2015 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn@buckwalter.se
   Stability  : Stable
   Portability: GHC only

= Summary

This module defines units that are not part of the SI, with the
exception of those defined in the "Numeric.Units.Dimensional.SIUnits" module (units outside
of the SI accepted for use with the SI).

Any chapters, sections or tables referenced are from <#note1 [1]> unless
otherwise specified.

== Neper, bel, shannon and the like

The units of section 5.1.2 are purposefully (but not permanently)
omitted. In fact the logarithmic units (see section 8.7) are
problematic and it is not clear how to implement them. Perhaps with
a conversion function similar to for degrees Celsius.

= References

1. #note1# http://physics.nist.gov/Pubs/SP811/
2. #note2# http://www.iau.org/science/publications/proceedings_rules/units/
3. #note3# http://en.m.wikipedia.org/wiki/Pressure
4. #note4# http://en.m.wikipedia.org/wiki/Torr

-}

module Numeric.Units.Dimensional.NonSI
(
  -- * Units Defined By Experiment
  -- $values-obtained-experimentally
  electronVolt, unifiedAtomicMassUnit, dalton,
  -- * Standard Gravity
  gee,
  -- * Inch-pound Units
  -- $inch-pound-units
  poundMass, ounce, poundForce, horsepower, btu, shortTon,
  nauticalMile, knot,
  revolution, solid,
  slug, psi,
  teaspoon,
  -- ** International Foot
  foot, inch, mil, yard, mile, acre,
  -- ** US Survey Foot
  usSurveyFoot, usSurveyInch, usSurveyMil, usSurveyYard, usSurveyMile, usSurveyAcre,
  -- * Years
  -- $year
  year, century,
  -- * Pressure Units
  -- $pressure-units
  bar, atmosphere, technicalAtmosphere, mmHg, inHg, inHg_UCUM, inHg_NIST, torr,
  -- * Radiation Units
  rad,
  -- * Kinematic Viscosity
  stokes,
  -- * Temperature
  -- $temperature
  degreeFahrenheit, degreeRankine,
  -- * Imperial Volumes
  -- $imperial-volumes
  imperialGallon, imperialQuart, imperialPint, imperialCup, imperialGill, imperialFluidOunce,
  -- * US Customary Volumes
  -- $us-customary-volumes
  usGallon, usQuart, usPint, usCup, usGill, usFluidOunce,
  -- * Atomic-Scale Units
  angstrom,
  -- * Units from the Centimeter-Gram-Second Electrostatic System of Units
  gauss
)
where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.UnitNames.Internal (atomic, metricAtomic, siunitx, ucum, internationalEnglishAscii)
import qualified Prelude

-- $setup
-- >>> import Data.ExactPi
-- >>> import Data.Function (on)
-- >>> import Numeric.Units.Dimensional.Coercion
-- >>> default (Double)
-- >>> :{
-- >>>   let infix 4 ===
-- >>>       (===) = areExactlyEqual `on` unQuantity :: Quantity d ExactPi -> Quantity d ExactPi -> Bool
-- >>> :}

{- $values-obtained-experimentally

From Table 7, units accepted for use with the SI whose values in SI units are
obtained experimentally.

When <#note1 [1]> was published the electron volt had a standard combined
uncertainity of 0.00000049e-19 J and the unified atomic mass unit
had a combined uncertainty of 0.0000010e-27 kg.

-}

electronVolt :: Floating a => Unit 'Metric DEnergy a
electronVolt = mkUnitR n 1.60217733e-19 joule
  where
    n = metricAtomic "eV" "eV" "electron volt" [(siunitx, "\\electronvolt")]

unifiedAtomicMassUnit :: Floating a => Unit 'Metric DMass a
unifiedAtomicMassUnit = mkUnitR n 1.6605402e-27 $ kilo gram
  where
    n = metricAtomic "u" "u" "atomic mass unit" [(siunitx, "\\atomicmassunit")]

dalton :: Floating a => Unit 'Metric DMass a
dalton = mkUnitR n 1 unifiedAtomicMassUnit
  where
    n = metricAtomic "u" "Da" "Dalton" [(siunitx, "\\dalton")]

-- | One gee is the standard value of the acceleration due to gravity at the
-- Earth's surface, as standardized by CIPM.
--
-- Note that local values of acceleration due to gravity will differ from the
-- standard gravity.
--
-- See <https://en.wikipedia.org/wiki/Standard_gravity here> for further information.
--
-- >>> 1 *~ gee
-- 9.80665 m s^-2
--
-- >>> 1 *~ gee :: Acceleration Rational
-- 196133 % 20000 m s^-2
gee :: Fractional a => Unit 'Metric DAcceleration a
gee = mkUnitQ n 9.80665 $ meter / second ^ pos2
  where
    n = metricAtomic "[g]" "g" "gee" []

{- $inch-pound-units
Some US customary (that is, inch-pound) units.
-}

-- | One international foot is one third of an international 'yard'.
--
-- See <https://en.wikipedia.org/wiki/Foot_%28unit%29#International_foot here> for further information.
--
-- >>> 1 *~ foot
-- 0.3048 m
--
-- prop> 3 *~ foot === 1 *~ yard
--
-- >>> 1 *~ foot :: Length Rational
-- 381 % 1250 m
foot :: Fractional a => Unit 'NonMetric DLength a
foot = mkUnitQ n (1 Prelude./ 3) yard
  where
    n = atomic "ft" "foot" [(ucum, "[ft_i]")]

-- | One inch is one twelth of a 'foot'.
--
-- This inch is based on the international 'foot'.
--
-- See <https://en.wikipedia.org/wiki/Inch#Modern_standardisation here> for further information.
--
-- >>> 1 *~ inch
-- 2.54e-2 m
--
-- prop> 12 *~ inch === 1 *~ foot
--
-- >>> 1 *~ inch :: Length Rational
-- 127 % 5000 m
inch :: Fractional a => Unit 'NonMetric DLength a
inch = mkUnitQ n (1 Prelude./ 12) foot
  where
    n = atomic "in" "inch" [(ucum, "[in_i]")]

-- | One mil is one thousandth of an 'inch'.
--
-- This mil is based on the international 'inch'.
--
-- See <https://en.wikipedia.org/wiki/Thousandth_of_an_inch here> for further information.
--
-- >>> 1 *~ mil
-- 2.54e-5 m
--
-- prop> 1000 *~ mil === 1 *~ inch
--
-- >>> 1 *~ mil :: Length Rational
-- 127 % 5000000 m
mil :: Fractional a => Unit 'NonMetric DLength a
mil = mkUnitQ n 0.001 inch
  where
    n = atomic "mil" "mil" [(ucum, "[mil_i]")]

-- | One yard, as defined by international agreement in 1959, is precisely
-- 0.9144 'meter'.
--
-- See <https://en.wikipedia.org/wiki/Yard here> for further information.
--
-- >>> 1 *~ yard
-- 0.9144 m
--
-- >>> 1 *~ yard :: Length Rational
-- 1143 % 1250 m
yard :: (Fractional a) => Unit 'NonMetric DLength a
yard = mkUnitQ n 0.9144 meter
  where
    n = atomic "yd" "yard" [(ucum, "[yd_i]")]

-- | One mile is 5 280 feet.
--
-- This mile is based on the international 'foot'.
--
-- See <https://en.wikipedia.org/wiki/Mile#International_mile here> for further information.
--
-- >>> 1 *~ mile
-- 1609.344 m
--
-- prop> 1 *~ mile === 5280 *~ foot
--
-- >>> 1 *~ mile :: Length Rational
-- 201168 % 125 m
mile :: (Fractional a) => Unit 'NonMetric DLength a
mile = mkUnitQ n 5280 foot
  where
    n = atomic "mi" "mile" [(ucum, "[mi_i]")]

-- | One acre is 43 560 square feet.
--
-- This acre is based on the international 'foot'. For the acre based on the US Survey Foot,
-- see 'usSurveyAcre'. While both acres are in use, the difference between them is of little consequence
-- for most applications in which either is used.
--
-- See <https://en.wikipedia.org/wiki/Acre#Differences_between_international_and_US_survey_acres here> for further information.
--
-- >>> 1 *~ acre
-- 4046.8564224 m^2
--
-- prop> 1 *~ acre === 43560 *~ foot ^ pos2
--
-- >>> 1 *~ acre :: Area Rational
-- 316160658 % 78125 m^2
acre :: (Fractional a) => Unit 'NonMetric DArea a
acre = mkUnitQ n 43560 $ square foot
  where
    n = atomic "ac" "acre" []

-- | One US survey foot is 1200/3937 'meter'.
--
-- For the international foot, see 'foot'. Note that this is not the foot in routine use
-- in the United States.
--
-- See <https://en.wikipedia.org/wiki/Foot_%28unit%29#US_survey_foot here> for further information.
--
-- >>> 1 *~ usSurveyFoot
-- 0.3048006096012192 m
--
-- >>> 1 *~ usSurveyFoot :: Length Rational
-- 1200 % 3937 m
usSurveyFoot :: Fractional a => Unit 'NonMetric DLength a
usSurveyFoot = mkUnitQ n (1200 Prelude./ 3937) meter
  where
    n = atomic "ft" "foot" [(ucum, "[ft_us]")]

-- | One inch is one twelth of a foot.
--
-- This inch is based on the 'usSurveyFoot'. For the inch based on the international foot,
-- see 'inch'. Note that this is not the inch in routine use in the United States.
--
-- See <https://en.wikipedia.org/wiki/Inch here> for further information.
--
-- >>> 1 *~ usSurveyInch
-- 2.54000508001016e-2 m
--
-- prop> 12 *~ usSurveyInch === 1 *~ usSurveyFoot
--
-- >>> 1 *~ usSurveyInch :: Length Rational
-- 100 % 3937 m
usSurveyInch :: Fractional a => Unit 'NonMetric DLength a
usSurveyInch = mkUnitQ n (1 Prelude./ 12) usSurveyFoot
  where
    n = atomic "in" "inch" [(ucum, "[in_us]")]

-- | One mil is one thousandth of an inch.
--
-- This mil is based on the 'usSurveyInch'. For the mil based on the international inch,
-- see 'mil'. Note that this is not the mil in routine use in the United States.
--
-- See <https://en.wikipedia.org/wiki/Thousandth_of_an_inch here> for further information.
--
-- >>> 1 *~ usSurveyMil
-- 2.54000508001016e-5 m
--
-- prop> 1000 *~ usSurveyMil === 1 *~ usSurveyInch
--
-- >>> 1 *~ usSurveyMil :: Length Rational
-- 1 % 39370 m
usSurveyMil :: Fractional a => Unit 'NonMetric DLength a
usSurveyMil = mkUnitQ n 0.001 usSurveyInch
  where
    n = atomic "mil" "mil" [(ucum, "[mil_us]")]

-- | One yard is three feet.
--
-- This yard is based on the 'usSurveyFoot'. For the international yard,
-- see 'yard'. Note that this is not the yard in routine use in the United States.
--
-- See <https://en.wikipedia.org/wiki/Yard here> for further information.
--
-- >>> 1 *~ usSurveyYard
-- 0.9144018288036576 m
--
-- prop> 1 *~ usSurveyYard === 3 *~ usSurveyFoot
--
-- >>> 1 *~ usSurveyYard :: Length Rational
-- 3600 % 3937 m
usSurveyYard :: (Fractional a) => Unit 'NonMetric DLength a
usSurveyYard = mkUnitQ n 3 usSurveyFoot
  where
    n = atomic "yd" "yard" [(ucum, "[yd_us]")]

-- | One US survey mile is 5 280 US survey feet.
--
-- This mile is based on the 'usSurveyFoot'. For the mile based on the international foot,
-- see 'mile'. Note that this is not the mile in routine use in the United States.
--
-- See <https://en.wikipedia.org/wiki/Mile#US_survey_mile here> for further information.
--
-- >>> 1 *~ usSurveyMile
-- 1609.3472186944373 m
--
-- prop> 1 *~ usSurveyMile === 5280 *~ usSurveyFoot
--
-- >>> 1 *~ usSurveyMile :: Length Rational
-- 6336000 % 3937 m
usSurveyMile :: (Fractional a) => Unit 'NonMetric DLength a
usSurveyMile = mkUnitQ n 5280 usSurveyFoot
  where
    n = atomic "mi" "mile" [(ucum, "[mi_us]")]

-- | One acre is 43 560 square feet.
--
-- This acre is based on the 'usSurveyFoot'. For the acre based on the international foot,
-- see 'acre'. While both acres are in use, the difference between them is of little consequence
-- for most applications in which either is used. This is the only acre defined by the UCUM.
--
-- See <https://en.wikipedia.org/wiki/Acre#Differences_between_international_and_US_survey_acres here> for further information.
--
-- >>> 1 *~ usSurveyAcre
-- 4046.872609874252 m^2
--
-- prop> 1 *~ usSurveyAcre === 43560 *~ usSurveyFoot ^ pos2
--
-- >>> 1 *~ usSurveyAcre :: Area Rational
-- 62726400000 % 15499969 m^2
usSurveyAcre :: (Fractional a) => Unit 'NonMetric DArea a
usSurveyAcre = mkUnitQ n 43560 $ square usSurveyFoot
  where
    n = atomic "ac" "acre" [(ucum, "[acr_us]")]

-- | One avoirdupois pound is a mass, exactly defined in terms of the kilogram by the international
-- yard and pound agreement of 1959.
--
-- See <https://en.wikipedia.org/wiki/Avoirdupois#Internationalization here> for further information.
--
-- >>> 1 *~ poundMass
-- 0.45359237 kg
--
-- >>> 1 *~ poundMass :: Mass Rational
-- 45359237 % 100000000 kg
poundMass :: Fractional a => Unit 'NonMetric DMass a
poundMass = mkUnitQ n 0.45359237 $ kilo gram
  where
    n = atomic "lb" "pound" [(ucum, "[lb_av]")]

-- | One avoirdupois ounce is one sixteenth of a 'poundMass'.
--
-- See <https://en.wikipedia.org/wiki/Ounce#International_avoirdupois_ounce here> for further information.
--
-- >>> 1 *~ ounce
-- 2.8349523125e-2 kg
--
-- prop> 16 *~ ounce === 1 *~ poundMass
--
-- >>> 1 *~ ounce :: Mass Rational
-- 45359237 % 1600000000 kg
ounce :: Fractional a => Unit 'NonMetric DMass a
ounce = mkUnitQ n (1 Prelude./ 16) poundMass
  where
    n = atomic "oz" "ounce" [(ucum, "[oz_av]")]

-- | One short ton is two thousand 'poundMass'.
--
-- See <https://en.wikipedia.org/wiki/Short_ton#United_States here> for further information.
--
-- >>> 1 *~ shortTon
-- 907.18474 kg
--
-- >>> 1 *~ shortTon :: Mass Rational
-- 45359237 % 50000 kg
shortTon :: Fractional a => Unit 'NonMetric DMass a
shortTon = mkUnitQ n 2000 poundMass
  where
    n = atomic "ton" "short ton" [(ucum, "[ston_av]")]

-- | The pound-force is equal to the gravitational force exerted on a mass
-- of one avoirdupois pound on the surface of Earth.
--
-- This definition is based on standard gravity (the 'gee') and the
-- international avoirdupois 'poundMass'.
--
-- See <https://en.wikipedia.org/wiki/Pound_%28force%29 here> for further information.
--
-- >>> 1 *~ poundForce
-- 4.4482216152605 m kg s^-2
--
-- prop> 1 *~ poundForce === 1 *~ poundMass * (1 *~ gee)
--
-- >>> 1 *~ poundForce :: Force Rational
-- 8896443230521 % 2000000000000 m kg s^-2
poundForce :: Fractional a => Unit 'NonMetric DForce a
poundForce = mkUnitQ n 1 $ poundMass * gee
  where
    n = atomic "lbf" "pound force" [(ucum, "[lbf_av]")]

-- | One mechanical horsepower is by definition the power necessary
-- to apply a force of 550 'poundForce' through a distance of one 'foot'
-- per 'second'.
--
-- See <https://en.wikipedia.org/wiki/Horsepower#Mechanical_horsepower here> for further information.
--
-- >>> 1 *~ horsepower
-- 745.6998715822702 m^2 kg s^-3
--
-- prop> 1 *~ horsepower === 550 *~ poundForce * (1 *~ foot) / (1 *~ second)
--
-- >>> 1 *~ horsepower :: Power Rational
-- 37284993579113511 % 50000000000000 m^2 kg s^-3
horsepower :: Fractional a => Unit 'NonMetric DPower a
horsepower = mkUnitQ n 550 $ foot * poundForce / second
  where
    n = atomic "hp" "horsepower" [(ucum, "[HP]")]

-- | The slug is a unit of mass associated with Imperial units and United States customary units.
-- It is a mass that accelerates by 1 foot per second per second when a force of one pound is exerted on it.
--
-- This definition is based on standard gravity (the 'gee'), the international 'foot', and the international avoirdupois 'poundMass'.
--
-- See <https://en.wikipedia.org/wiki/Slug_%28mass%29 here> for further information.
--
-- >>> 1 *~ slug
-- 14.593902937206364 kg
--
-- >>> 1 *~ slug :: Mass Rational
-- 8896443230521 % 609600000000 kg
slug :: Fractional a => Unit 'NonMetric DMass a
slug = mkUnitQ n 1 $ poundForce * (second^pos2) / foot
  where
    n = atomic "slug" "slug" []

-- | One psi is a pressure of one 'poundForce' per 'square' 'inch' of area.
--
-- See <https://en.wikipedia.org/wiki/Pounds_per_square_inch here> for further information.
--
-- >>> 1 *~ psi
-- 6894.757293168362 m^-1 kg s^-2
--
-- >>> 1 *~ psi :: Pressure Rational
-- 8896443230521 % 1290320000 m^-1 kg s^-2
psi :: Fractional a => Unit 'NonMetric DPressure a
psi = mkUnitQ n 1 $ poundForce / inch ^ pos2
  where
    n = atomic "psi" "pound per square inch" [(ucum, "[psi]")]

-- | One nautical mile is a unit of length, set by international agreement as being exactly 1 852 meters.
--
-- Historically, it was defined as the distance spanned by one minute of arc along a meridian of the Earth.
--
-- See <https://en.wikipedia.org/wiki/Nautical_mile here> for further information.
--
-- >>> 1 *~ nauticalMile
-- 1852.0 m
--
-- >>> 1 *~ nauticalMile :: Length Rational
-- 1852 % 1 m
nauticalMile :: (Num a) => Unit 'NonMetric DLength a
nauticalMile = mkUnitZ n 1852 meter
  where
    n = atomic "NM" "nautical mile" [(ucum, "[nmi_i]"),
                                     (siunitx, "\\nauticalmile")]

-- | One knot is a velocity equal to one 'nauticalMile' per 'hour'.
--
-- See <https://en.wikipedia.org/wiki/Knot_%28unit%29 here> for further information.
--
-- >>> 1 *~ knot
-- 0.5144444444444445 m s^-1
--
-- >>> 1 *~ knot :: Velocity Rational
-- 463 % 900 m s^-1
knot :: (Fractional a) => Unit 'NonMetric DVelocity a
knot = mkUnitQ n 1 $ nauticalMile / hour
  where
    n = atomic "kt" "knot" [(ucum, "[kt_i]")]

-- | One revolution is an angle equal to 2 pi radians; a full circle.
--
-- See <https://en.wikipedia.org/wiki/Turn_%28geometry%29 here> for further information.
--
-- >>> 1 *~ revolution
-- 6.283185307179586
--
-- prop> 1 *~ revolution === _2 * pi * (1 *~ radian)
--
-- prop> 1 *~ revolution === 360 *~ degree
revolution :: (Floating a) => Unit 'NonMetric DOne a
revolution = mkUnitR n (2 Prelude.* Prelude.pi) radian
  where
    n = atomic "rev" "revolution" []

solid :: (Floating a) => Unit 'NonMetric DOne a
solid = mkUnitR n (4 Prelude.* Prelude.pi) steradian
  where
    n = atomic "solid" "solid" []

teaspoon :: (Fractional a) => Unit 'NonMetric DVolume a
teaspoon = mkUnitQ n 5 $ milli liter
  where
    n = atomic "tsp" "teaspoon" [(ucum, "[tsp_m]")]

-- | One btu is is the 'QuantityOfHeat' required to raise the temperature
-- of 1 avoirdupois 'poundMass' of liquid water by 1 'degreeFahrenheit' at a constant pressure of one 'atmosphere'.
--
-- Because this value must be determined experimentally and varies with temperature, several standardized
-- values of the btu have arisen. This is the value based on the International Steam Table calorie,
-- defined by the Fifth International Conference on the Properties of Steam.
--
-- See <https://en.wikipedia.org/wiki/British_thermal_unit#Definitions here> for further information.
--
-- >>> 1 *~ btu
-- 1055.05585262 m^2 kg s^-2
--
-- >>> 1 *~ btu :: Energy Rational
-- 52752792631 % 50000000 m^2 kg s^-2
btu :: Fractional a => Unit 'NonMetric DEnergy a
btu = mkUnitQ n 1055.05585262 joule
  where
    n = atomic "btu" "British thermal unit" [(ucum, "[Btu_IT]")]

{- $year

The IAU recommends <#note2 [2]> that:

  Although there are several different kinds of year (as there are
  several kinds of day), it is best to regard a year as a julian
  year of 365.25 days (31.5576 Ms) unless otherwise specified.

-}

-- | One mean Julian year is a unit of measurement of time defined as exactly 365.25 days of 86 400 'second's each.
--
-- See <https://en.wikipedia.org/wiki/Julian_year_%28astronomy%29 here> for further information.
--
-- >>> 1 *~ year
-- 3.15576e7 s
--
-- >>> 1 *~ year :: Time Rational
-- 31557600 % 1 s
year :: Num a => Unit 'NonMetric DTime a
year = mkUnitZ n 31557600 second
  where
    n = atomic "a" "mean Julian year" [(ucum, "a_j")]

-- | One mean Julian century is one hundred mean Julian 'year's.
--
-- >>> 1 *~ century
-- 3.15576e9 s
--
-- >>> 1 *~ century :: Time Rational
-- 3155760000 % 1 s
century :: Num a => Unit 'NonMetric DTime a
century = mkUnitZ n 100 year
  where
    n = atomic "cen" "mean Julian century" []

{- $pressure-units
It seems that nearly every area of application has its own customary unit for measuring pressure.
We include some of the common ones here. 'psi' was defined earlier.
-}

-- | The bar is exactly 100 000 'Numeric.Units.Dimensional.SIUnits.pascal'.
--
-- From Wikipedia:
--
--  It is about equal to the atmospheric pressure on Earth at sea level.
--
-- >>> 1 *~ bar
-- 100000.0 m^-1 kg s^-2
--
-- >>> 1 *~ bar :: Pressure Rational
-- 100000 % 1 m^-1 kg s^-2
bar :: (Num a) => Unit 'Metric DPressure a
bar = mkUnitZ n 1e5 pascal
  where
    n = metricAtomic "bar" "bar" "bar" [(siunitx, "\\bar")]

-- | The "standard atmosphere".
--
-- From Wikipedia <#note3 [3]>:
--
--  The standard atmosphere (atm) is an established constant. It is
--  approximately equal to typical air pressure at earth mean sea
--  level.
--
-- >>> 1 *~ atmosphere
-- 101325.0 m^-1 kg s^-2
--
-- >>> 1 *~ atmosphere :: Pressure Rational
-- 101325 % 1 m^-1 kg s^-2
atmosphere :: (Num a) => Unit 'NonMetric DPressure a
atmosphere = mkUnitZ n 101325 pascal
  where
    n = atomic "atm" "standard atmosphere" [(ucum, "atm")]

-- | The "technical atmosphere"
--
-- From Wikipedia:
--
--  A technical atmosphere (symbol: at) is a non-SI unit of pressure equal
--  to one kilogram-force per square centimeter.
--
-- >>> 1 *~ technicalAtmosphere
-- 98066.5 m^-1 kg s^-2
--
-- >>> 1 *~ technicalAtmosphere :: Pressure Rational
-- 196133 % 2 m^-1 kg s^-2
technicalAtmosphere :: (Fractional a) => Unit 'NonMetric DPressure a
technicalAtmosphere = mkUnitQ n 1 $ kilo gram * gee * centi meter ^ neg2
  where
    n = atomic "at" "technical atmosphere" [(ucum, "att")]

-- | The conventional value for the pressure exerted by a 1 mm high column of mercury.
--
-- Per Wikipedia <#note4 [4]>, one mmHg (millimeter of mercury) is defined as:
--
--  The pressure exerted at the base of a column of fluid exactly 1 mm high,
--  when the density of the fluid is exactly 13.5951 g/cm^3, at a place
--  where the acceleration of gravity is exactly 9.80665 m/s^2.
--
-- The chosen fluid density approximately corresponds to that of mercury
-- at 0 deg. Under most conditions, 1 mmHg is approximately equal to 1 'torr'.
--
-- >>> 1 *~ mmHg
-- 133.322 m^-1 kg s^-2
--
-- >>> 1 *~ mmHg :: Pressure Rational
-- 66661 % 500 m^-1 kg s^-2
mmHg :: (Fractional a) => Unit 'NonMetric DPressure a
mmHg = milli mHg

mHg :: (Fractional a) => Unit 'Metric DPressure a
mHg = mkUnitQ n 133.3220 $ kilo pascal
  where
    n = metricAtomic "m[Hg]" "m Hg" "meter of mercury" []

-- | The conventional value for the pressure exerted by a 1 inch high column of mercury.
--
-- Column inches of mercury are also used to measure pressure, especially in
-- meteorological or aeronautical contexts in the United States.
--
-- This is the value defined by UCUM. For the value defined by NIST, see 'inHg_NIST'.
--
-- >>> 1 *~ inHg
-- 3386.3788 m^-1 kg s^-2
--
-- >>> 1 *~ inHg :: Pressure Rational
-- 8465947 % 2500 m^-1 kg s^-2
inHg :: (Fractional a) => Unit 'NonMetric DPressure a
inHg = inHg_UCUM

-- | The conventional value for the pressure exerted by a 1 inch high column of mercury.
--
-- Column inches of mercury are also used to measure pressure, especially in
-- meteorological or aeronautical contexts in the United States.
--
-- This is the value defined by UCUM. For the value defined by NIST, see 'inHg_NIST'.
--
-- >>> 1 *~ inHg_UCUM
-- 3386.3788 m^-1 kg s^-2
--
-- >>> 1 *~ inHg_UCUM :: Pressure Rational
-- 8465947 % 2500 m^-1 kg s^-2
inHg_UCUM :: (Fractional a) => Unit 'NonMetric DPressure a
inHg_UCUM = mkUnitQ n 1 $ mHg * inch / meter
  where
    n = atomic "in Hg" "inch of mercury" [(ucum, "[in_i'Hg]")]

-- | The conventional value for the pressure exerted by a 1 inch high column of mercury.
--
-- Column inches of mercury are also used to measure pressure, especially in
-- meteorological or aeronautical contexts in the United States.
--
-- This is the value defined by NIST. For the value defined by UCUM, see 'inHg_UCUM'.
--
-- >>> 1 *~ inHg_NIST
-- 3386.389 m^-1 kg s^-2
--
-- >>> 1 *~ inHg_NIST :: Pressure Rational
-- 3386389 % 1000 m^-1 kg s^-2
inHg_NIST :: (Fractional a) => Unit 'NonMetric DPressure a
inHg_NIST = mkUnitQ n 3.386389e3 pascal
  where
    n = atomic "in Hg" "inch of mercury" []

-- | One torr (symbol: Torr) is defined as 1/760 'atmosphere', which is approximately equal to 1 'mmHg'.
--
-- See <https://en.wikipedia.org/wiki/Torr here> for further information.
--
-- >>> 1 *~ torr
-- 133.32236842105263 m^-1 kg s^-2
--
-- >>> 1 *~ torr :: Pressure Rational
-- 20265 % 152 m^-1 kg s^-2
torr :: (Fractional a) => Unit 'NonMetric DPressure a
torr = mkUnitQ n (1 Prelude./ 760) atmosphere
  where
    n = atomic "Torr" "Torr" [(siunitx, "\torr")]

-- | The rad is a deprecated unit of 'AbsorbedDose', defined as
-- 0.01 'gray'.
--
-- See <https://en.wikipedia.org/wiki/Rad_%28unit%29 here> for further information.
--
-- >>> 1 *~ rad
-- 1.0e-2 m^2 s^-2
--
-- >>> 1 *~ rad :: AbsorbedDose Rational
-- 1 % 100 m^2 s^-2
rad :: (Fractional a) => Unit 'Metric DAbsorbedDose a
rad = mkUnitQ n 1 $ centi gray
  where
    n = metricAtomic "RAD" "RAD" "RAD" []

-- | One Stokes is a unit of 'KinematicViscosity' equal to @1 cm^2 / s@.
--
-- See <https://en.wikipedia.org/wiki/Viscosity#Kinematic_viscosity_.CE.BD here> for further information.
--
-- >>> 1 *~ stokes
-- 1.0e-4 m^2 s^-1
--
-- >>> 1 *~ stokes :: KinematicViscosity Rational
-- 1 % 10000 m^2 s^-1
stokes :: (Fractional a) => Unit 'Metric DKinematicViscosity a
stokes = mkUnitQ n 1 $ centi meter ^ pos2 / second
  where
    n = metricAtomic "St" "St" "Stokes" []

{- $temperature
These units of temperature are relative. For absolute temperatures, see 'Numeric.Units.Dimensional.SIUnits.fromDegreeCelsiusAbsolute'.
-}

-- | One degree Fahrenheit is a unit of relative temperature equal to 5/9 'kelvin'.
--
-- Note that although the Fahrenheit scale is an absolute temperature scale, this unit is a unit of difference within
-- that scale and measures relative temperature.
--
-- See <https://en.wikipedia.org/wiki/Fahrenheit#Definition_and_conversions here> for further information.
--
-- >>> 1 *~ degreeFahrenheit
-- 0.5555555555555556 K
--
-- >>> 1 *~ degreeFahrenheit :: ThermodynamicTemperature Rational
-- 5 % 9 K
degreeFahrenheit :: (Fractional a) => Unit 'NonMetric DThermodynamicTemperature a
degreeFahrenheit = mkUnitQ n (5 Prelude./ 9) degreeCelsius
  where
    n = atomic "°F" "degree Fahrenheit" [(ucum, "[degF]")]

-- | One degree Rankine is a unit of relative temperature equal to 5/9 'kelvin'.
--
-- Note that although the Rankine scale is an absolute temperature scale, this unit is a unit of difference within
-- that scale and measures relative temperature.
--
-- See <https://en.wikipedia.org/wiki/Rankine_scale here> for further information.
--
-- >>> 1 *~ degreeRankine
-- 0.5555555555555556 K
--
-- >>> 1 *~ degreeRankine :: ThermodynamicTemperature Rational
-- 5 % 9 K
degreeRankine :: (Fractional a) => Unit 'NonMetric DThermodynamicTemperature a
degreeRankine = mkUnitQ n 1 degreeFahrenheit
  where
    n = atomic "°R" "degree Rankine" [(ucum, "[degR]")]

{- $imperial-volumes
Per http://en.wikipedia.org/wiki/Imperial_units and http://en.wikipedia.org/wiki/Cup_(unit)#Imperial_cup.
-}

-- | One imperial gallon is defined exactly in terms of the 'liter'
-- by the Weights and Measures Act 1985.
--
-- See <https://en.wikipedia.org/wiki/Imperial_units#Volume here> for further information.
--
-- >>> 1 *~ imperialGallon
-- 4.54609e-3 m^3
--
-- >>> 1 *~ imperialGallon :: Volume Rational
-- 454609 % 100000000 m^3
imperialGallon :: (Fractional a) => Unit 'NonMetric DVolume a
imperialGallon = mkUnitQ n 4.54609 liter
  where
    n = atomic "gal" "gallon" [(ucum, "[gal_br]")]

-- | One imperial quart is one quarter of an 'imperialGallon'.
--
-- See <https://en.wikipedia.org/wiki/Imperial_units#Volume here> for further information.
--
-- >>> 1 *~ imperialQuart
-- 1.1365225e-3 m^3
--
-- >>> 1 *~ imperialQuart :: Volume Rational
-- 454609 % 400000000 m^3
imperialQuart :: (Fractional a) => Unit 'NonMetric DVolume a
imperialQuart = mkUnitQ n (1 Prelude./ 4) imperialGallon
  where
    n = atomic "qt" "quart" [(ucum, "[qt_br]")]

-- | One imperial pint is one half of an 'imperialQuart'.
--
-- See <https://en.wikipedia.org/wiki/Imperial_units#Volume here> for further information.
--
-- >>> 1 *~ imperialPint
-- 5.6826125e-4 m^3
--
-- >>> 1 *~ imperialPint :: Volume Rational
-- 454609 % 800000000 m^3
imperialPint :: (Fractional a) => Unit 'NonMetric DVolume a
imperialPint = mkUnitQ n (1 Prelude./ 8) imperialGallon
  where
    n = atomic "pt" "pint" [(ucum, "[pt_br]")]

-- | One imperial cup is one half of an 'imperialPint'.
--
-- This unit is not in common use and is does not appear in some sources
-- describing the imperial fluid volume units.
--
-- See <https://en.wikipedia.org/wiki/Cup_%28unit%29#Imperial_cup here> for further information.
--
-- >>> 1 *~ imperialCup
-- 2.84130625e-4 m^3
--
-- >>> 1 *~ imperialCup :: Volume Rational
-- 454609 % 1600000000 m^3
imperialCup :: (Fractional a) => Unit 'NonMetric DVolume a
imperialCup = mkUnitQ n 0.5 imperialPint
  where
    n = atomic "cup" "cup" []

-- | One imperial gill is one quarter of an 'imperialPint'.
--
-- See <https://en.wikipedia.org/wiki/Imperial_units#Volume here> for further information.
--
-- >>> 1 *~ imperialGill
-- 1.420653125e-4 m^3
--
-- >>> 1 *~ imperialGill :: Volume Rational
-- 454609 % 3200000000 m^3
imperialGill :: (Fractional a) => Unit 'NonMetric DVolume a
imperialGill = mkUnitQ n (1 Prelude./ 4) imperialPint
  where
    n = atomic "gill" "gill" [(ucum, "[gil_br]")]

-- | One imperial fluid ounce is one twentieth of an 'imperialPint'.
--
-- See <https://en.wikipedia.org/wiki/Imperial_units#Volume here> for further information.
--
-- >>> 1 *~ imperialFluidOunce
-- 2.84130625e-5 m^3
--
-- >>> 1 *~ imperialFluidOunce :: Volume Rational
-- 454609 % 16000000000 m^3
imperialFluidOunce :: (Fractional a) => Unit 'NonMetric DVolume a
imperialFluidOunce = mkUnitQ n (1 Prelude./ 20) imperialPint
  where
    n = atomic "fl oz" "fluid ounce" [(ucum, "[foz_br]")]

{- $us-customary-volumes
Per http://www.nist.gov/pml/wmd/pubs/upload/2012-hb44-final.pdf page 452 and http://en.wikipedia.org/wiki/United_States_customary_units#Fluid_volume
Note that there exist rarely-used "dry" variants of units with overlapping names.
-}

-- | One US liquid gallon is a volume of 231 cubic inches.
--
-- See <https://en.wikipedia.org/wiki/Gallon#The_US_liquid_gallon here> for further information.
--
-- >>> 1 *~ usGallon
-- 3.785411784e-3 m^3
--
-- >>> 1 *~ usGallon :: Volume Rational
-- 473176473 % 125000000000 m^3
usGallon :: (Fractional a) => Unit 'NonMetric DVolume a
usGallon = mkUnitQ n 231 $ cubic inch
  where
    n = atomic "gal" "gallon" [(ucum, "[gal_us]")]

-- | One US liquid quart is one quarter of a 'usGallon'.
--
-- See <https://en.wikipedia.org/wiki/United_States_customary_units#Fluid_volume here> for further information.
--
-- >>> 1 *~ usQuart
-- 9.46352946e-4 m^3
--
-- >>> 1 *~ usQuart :: Volume Rational
-- 473176473 % 500000000000 m^3
usQuart :: (Fractional a) => Unit 'NonMetric DVolume a
usQuart = mkUnitQ n (1 Prelude./ 4) usGallon
  where
    n = atomic "qt" "quart" [(ucum, "[qt_us]")]

-- | One US liquid pint is one half of a 'usQuart'.
--
-- See <https://en.wikipedia.org/wiki/United_States_customary_units#Fluid_volume here> for further information.
--
-- >>> 1 *~ usPint
-- 4.73176473e-4 m^3
--
-- >>> 1 *~ usPint :: Volume Rational
-- 473176473 % 1000000000000 m^3
usPint :: (Fractional a) => Unit 'NonMetric DVolume a
usPint = mkUnitQ n (1 Prelude./ 8) usGallon
  where
    n = atomic "pt" "pint" [(ucum, "[pt_us]")]

-- | One US liquid cup is one half of a 'usPint'.
--
-- See <https://en.wikipedia.org/wiki/United_States_customary_units#Fluid_volume here> for further information.
--
-- >>> 1 *~ usCup
-- 2.365882365e-4 m^3
--
-- >>> 1 *~ usCup :: Volume Rational
-- 473176473 % 2000000000000 m^3
usCup :: (Fractional a) => Unit 'NonMetric DVolume a
usCup = mkUnitQ n (1 Prelude./ 2) usPint
  where
    n = atomic "cup" "cup" [(ucum, "[cup_us]")]

-- | One US liquid gill is one half of a 'usCup'.
--
-- See <https://en.wikipedia.org/wiki/United_States_customary_units#Fluid_volume here> for further information.
--
-- >>> 1 *~ usGill
-- 1.1829411825e-4 m^3
--
-- >>> 1 *~ usGill :: Volume Rational
-- 473176473 % 4000000000000 m^3
usGill :: (Fractional a) => Unit 'NonMetric DVolume a
usGill = mkUnitQ n (1 Prelude./ 4) usPint
  where
    n = atomic "gill" "gill" [(ucum, "[gil_us]")]

-- | One US fluid ounce is 1/128 'usGallon' or 1/8 'usCup'.
--
-- See <https://en.wikipedia.org/wiki/United_States_customary_units#Fluid_volume here> for further information.
--
-- >>> 1 *~ usFluidOunce
-- 2.95735295625e-5 m^3
--
-- >>> 1 *~ usFluidOunce :: Volume Rational
-- 473176473 % 16000000000000 m^3
usFluidOunce :: (Fractional a) => Unit 'NonMetric DVolume a
usFluidOunce = mkUnitQ n (1 Prelude./ 16) usPint -- sic, does not match factor used in imperial system
  where
    n = atomic "fl oz" "fluid ounce" [(ucum, "[foz_us]")]

-- | One Ångström is 1/10 'nano' 'meter'.
--
-- See <https://en.wikipedia.org/wiki/%C3%85ngstr%C3%B6m here> for further information.
--
-- >>> 1 *~ angstrom
-- 1.0e-10 m
--
-- >>> 1 *~ angstrom :: Length Rational
-- 1 % 10000000000 m
angstrom :: (Fractional a) => Unit 'NonMetric DLength a
angstrom = mkUnitQ n 0.1 $ nano meter
  where
    n = atomic "Å" "Ångström" [(ucum, "Ao"),
                               (siunitx, "\\angstrom"),
                               (internationalEnglishAscii, "Angstrom")]

-- | One Gauss is 1/10000 'tesla'.
--
-- See <https://en.wikipedia.org/wiki/Gauss_%28unit%29 here> for further information.
--
-- >>> 1 *~ gauss
-- 1.0e-4 kg s^-2 A^-1
--
-- >>> 1 *~ gauss :: MagneticFluxDensity Rational
-- 1 % 10000 kg s^-2 A^-1
gauss :: (Fractional a) => Unit 'NonMetric DMagneticFluxDensity a
gauss = mkUnitQ n 1e-4 tesla
  where
    n = atomic "G" "Gauss" [(ucum, "G")]
