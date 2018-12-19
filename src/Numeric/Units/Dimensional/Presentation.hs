{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.Units.Dimensional.Presentation
(
  -- * Presentation Quantities
  PresentationQuantity(..)
  -- * Presentation Formats
, PresentationUnit(..)
, simpleUnit
  -- * Presentation Numbers
, PresentationNumber(..)
, PresentationNumberFormat(..)
, value
, factorForDisplay
, presentValueIn
  -- * Presentation
  -- * Analysis
, analyze
)
where

import Data.Data
import Data.ExactPi (ExactPi(Exact), approximateValue)
import GHC.Generics
import Numeric.Natural
import Numeric.Units.Dimensional (dmap)
import Numeric.Units.Dimensional.Prelude hiding (exponent)
import qualified Prelude as P

data PresentationQuantity d = Simple PresentationNumber (Unit 'NonMetric d ExactPi)
                            | Composite Integer (Unit 'NonMetric d ExactPi) (PresentationQuantity d)
  deriving (Generic, Typeable)

data PresentationNumber = PresentationNumber 
  { piExponent :: Integer
  , number :: Either Rational (Integer, Int)
  , exponent :: Maybe (Natural, Integer)
  }

value :: PresentationNumber -> ExactPi
value x = Exact (piExponent x) q
  where
    q = q' (number x) P.* e (exponent x)
    q' :: (Either Rational (Integer, Int)) -> Rational
    q' (Left q'') = q''
    q' (Right (dm, dp)) = fromInteger dm P./ (10 P.^ dp)
    e :: Maybe (Natural, Integer) -> Rational
    e Nothing = 1
    e (Just (b, e')) = fromIntegral b P.^ e'

data PresentationNumberFormat a where
  ExactFormat :: PresentationNumberFormat ExactPi
  DecimalFormat :: (RealFloat a) => (Maybe Int) -> PresentationNumberFormat a

presentValueIn :: PresentationNumberFormat a -> a -> PresentationNumber
presentValueIn = undefined

factorForDisplay :: Integer -> (Integer, Integer, Integer) -- 10s, 2s, remainder
factorForDisplay 0 = (0, 0, 0)
factorForDisplay n = findTwos . findTens $ (0, 0, n)
  where
    findTens (ten, two, x) | (x', 0) <- x `quotRem` 10 = findTens (ten P.+ 1, two, x')
                           | otherwise = (ten, two, x)
    findTwos (ten, two, x) | (x', 0) <- x `quotRem` 2 = findTwos (ten, two P.+ 1, x')
                           | otherwise = (ten, two, x)

data PresentationUnit d = SimpleUnit (Unit 'NonMetric d ExactPi)
                        | PrefixedUnit (Unit 'Metric d ExactPi)
                        | PrefixedUnitMajor (Unit 'Metric d ExactPi)

analyze :: PresentationQuantity d -> Quantity d ExactPi
analyze (Simple x u) = value x *~ u
analyze (Composite x u q) = ipart + if (x < 0) then negate fpart else fpart
  where
    ipart = fromInteger x *~ u
    fpart = analyze q

simpleUnit :: Unit m d a -> PresentationUnit d
simpleUnit = SimpleUnit . weaken . exactify

chooseUnit :: (RealFrac a, Floating a) => PresentationUnit d -> Quantity d a -> Unit 'NonMetric d ExactPi
chooseUnit (SimpleUnit u)        _ = u
chooseUnit (PrefixedUnit u)      q = exactify $ withAppropriatePrefix (dmap approximateValue u) q
chooseUnit (PrefixedUnitMajor u) q = exactify $ withAppropriatePrefix' majorSiPrefixes (dmap approximateValue u) q
