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
  -- * Presentation Units
, PresentationUnit(..)
, simpleUnit
, prefixedUnit
, siPrefixedUnit
, majorSiPrefixedUnit
  -- * Presentation Formats
, PresentationFormat(..)
  -- * Presentation Numbers
, PresentationNumber(..)
, PresentationNumberFormat(..)
, value
, factorForDisplay
, presentValueIn
  -- * Presentation
, presentIn
  -- * Analysis
, analyze
)
where

import Data.Data
import Data.ExactPi (ExactPi(Exact))
import Data.List (splitAt)
import GHC.Generics
import Numeric.Natural
import Numeric.Units.Dimensional.Prelude hiding (exponent)
import Numeric.Units.Dimensional.UnitNames (PrefixSet, siPrefixes, majorSiPrefixes)
import qualified Prelude as P

data PresentationQuantity d = Simple PresentationNumber (Unit 'NonMetric d ExactPi)
                            | Composite Integer (Unit 'NonMetric d ExactPi) (PresentationQuantity d)
  deriving (Generic, Typeable)

instance Show (PresentationQuantity d) where
  show (Simple x u) = show x ++ "\xA0" ++ (show $ name u)
  show (Composite n u x) = show n ++ "\xA0" ++ (show $ name u) ++ "\xA0" ++ show x

data PresentationNumber = PresentationNumber 
  { piExponent :: Integer
  , number :: Either Rational (Integer, Int)
  , exponent :: Maybe (Natural, Integer)
  }

instance Show PresentationNumber where
  show (PresentationNumber 0 (Right (n, 0)) Nothing) = show n
  show (PresentationNumber 0 (Right (n, d)) Nothing) = reverse (f ++ ('.' : i))
    where
      (f, i) = splitAt d n'
      n' = reverse $ show n

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
  DecimalFormat :: (RealFrac a) => Int -> PresentationNumberFormat a

presentValueIn :: PresentationNumberFormat a -> a -> PresentationNumber
presentValueIn (DecimalFormat d) x = PresentationNumber { piExponent = 0, number = Right (x', d), exponent = Nothing }
  where
    x' = P.round $ x P.* (10 P.^ d)

factorForDisplay :: Integer -> (Integer, Integer, Integer) -- 10s, 2s, remainder
factorForDisplay 0 = (0, 0, 0)
factorForDisplay n = findTwos . findTens $ (0, 0, n)
  where
    findTens (ten, two, x) | (x', 0) <- x `quotRem` 10 = findTens (ten P.+ 1, two, x')
                           | otherwise = (ten, two, x)
    findTwos (ten, two, x) | (x', 0) <- x `quotRem` 2 = findTwos (ten, two P.+ 1, x')
                           | otherwise = (ten, two, x)

data PresentationUnit d = SimpleUnit (Unit 'NonMetric d ExactPi)
                        | PrefixedUnit PrefixSet (Unit 'Metric d ExactPi)

data PresentationFormat d a = SimpleFormat (PresentationNumberFormat a) (PresentationUnit d)
                            | CompositeFormat (Unit 'NonMetric d ExactPi) (PresentationFormat d a)

presentIn :: (RealFrac a, Floating a) => PresentationFormat d a -> Quantity d a -> PresentationQuantity d
presentIn (SimpleFormat nf u) q = Simple x' u'
  where
    u' = chooseUnit u q
    u'' = changeRepApproximate u'
    x = q /~ u''
    x' = presentValueIn nf x
presentIn (CompositeFormat u f) q = Composite n u pq
  where
    u' = changeRepApproximate u
    x = q /~ u'
    (n, x') = properFraction x
    q' = abs $ x' *~ u'
    pq = presentIn f q'

analyze :: PresentationQuantity d -> Quantity d ExactPi
analyze (Simple x u) = value x *~ u
analyze (Composite x u q) = ipart + if (x < 0) then negate fpart else fpart
  where
    ipart = fromInteger x *~ u
    fpart = analyze q

simpleUnit :: Unit m d a -> PresentationUnit d
simpleUnit = SimpleUnit . weaken . exactify

prefixedUnit :: PrefixSet -> Unit 'Metric d a -> PresentationUnit d
prefixedUnit ps = PrefixedUnit ps . exactify

siPrefixedUnit :: Unit 'Metric d a -> PresentationUnit d
siPrefixedUnit = prefixedUnit siPrefixes

majorSiPrefixedUnit :: Unit 'Metric d a -> PresentationUnit d
majorSiPrefixedUnit = prefixedUnit majorSiPrefixes

chooseUnit :: (RealFrac a, Floating a) => PresentationUnit d -> Quantity d a -> Unit 'NonMetric d ExactPi
chooseUnit (SimpleUnit u)        _ = u
chooseUnit (PrefixedUnit ps u)   q = exactify $ withAppropriatePrefix' ps (changeRepApproximate u) q
