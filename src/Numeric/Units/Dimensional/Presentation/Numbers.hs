{-# LANGUAGE GADTs #-}

module Numeric.Units.Dimensional.Presentation.Numbers
(
-- * Presentation Numbers
  PresentationNumber(..)
, value
-- * Presentation Number Formats
, PresentationNumberFormat(..)
, presentValueIn
)
where

import Prelude hiding (exponent)
import Data.ExactPi (ExactPi(Exact), approximateValue)
import Data.Ratio (numerator, denominator)
import Numeric.Natural (Natural)

-- | A 'PresentationNumber' is a number suitable for presentation to a human.
data PresentationNumber = PresentationNumber 
  { piExponent :: Integer
  , number :: Either Rational (Integer, Int)
  , exponent :: Maybe (Natural, Integer)
  }

-- | Converts a 'PresentationNumber' to an equivalent 'ExactPi' value by erasing presentational choices.
value :: PresentationNumber -> ExactPi
value x = Exact (piExponent x) q
  where
    q = q' (number x) * e (exponent x)
    q' :: (Either Rational (Integer, Int)) -> Rational
    q' (Left q'') = q''
    q' (Right (dm, dp)) = fromInteger dm / (10 ^ dp)
    e :: Maybe (Natural, Integer) -> Rational
    e Nothing = 1
    e (Just (b, e')) = fromIntegral b ^ e'

data PresentationNumberFormat a where
  ExactFormat :: PresentationNumberFormat ExactPi
  RationalFormat :: PresentationNumberFormat Rational
  DecimalFormat :: (RealFrac a) => Int -> PresentationNumberFormat a
  
presentValueIn :: PresentationNumberFormat a -> a -> PresentationNumber
presentValueIn ExactFormat (Exact z q) = PresentationNumber { piExponent = z, number = toFiniteDecimal q, exponent = Nothing }
presentValueIn RationalFormat x = PresentationNumber { piExponent = 0, number = Left x, exponent = Nothing }
presentValueIn (DecimalFormat d) x = PresentationNumber { piExponent = 0, number = Right (x', d), exponent = Nothing }
  where
    x' = round $ x * (10 ^ d)
  
-- | Converts a 'Rational' number to its finite decimal expansion, if it has one, or leaves it in rational form if it does not.
toFiniteDecimal :: Rational -> Either Rational (Integer, Int)
toFiniteDecimal x | (five, two, 1) <- factorForDisplay (denominator x) = Right (round $ x * (10 ^ (max five two)), fromIntegral $ max five two)
                  | otherwise = Left x

factorForDisplay :: Integer -> (Integer, Integer, Integer) -- 5s, 2s, remainder
factorForDisplay 0 = (0, 0, 0)
factorForDisplay n = findTwos . findFives $ (0, 0, n)
  where
    findFives (five, two, x) | (x', 0) <- x `quotRem` 5 = findFives (five + 1, two, x')
                              | otherwise = (five, two, x)
    findTwos (five, two, x) | (x', 0) <- x `quotRem` 2 = findTwos (five, two + 1, x')
                            | otherwise = (five, two, x)

instance Show PresentationNumber where
  show (PresentationNumber 0 (Left q) Nothing) = show n ++ " / " ++ show d
    where
      n = numerator q
      d = denominator q
  show (PresentationNumber 0 (Right (n, 0)) Nothing) = show n
  show (PresentationNumber 0 (Right (n, d)) Nothing) = reverse (f' ++ ('.' : i'))
    where
      i' = if i == [] then ['0'] else i
      f' = take d $ f ++ repeat '0'
      (f, i) = splitAt d n'
      n' = reverse $ show n
