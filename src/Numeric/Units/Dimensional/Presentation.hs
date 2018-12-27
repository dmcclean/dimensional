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
, PresentationFormat(..)
, decimals
  -- * Presentation
, presentIn
  -- * Analysis
, analyze
  -- * Presentation Numbers
, module Numeric.Units.Dimensional.Presentation.Numbers
  -- * Presentation Units
, module Numeric.Units.Dimensional.Presentation.Units
)
where

import Data.Data
import Data.ExactPi (ExactPi, approximateValue)
import Data.List.NonEmpty (cons, uncons, zip, zipWith)
import GHC.Generics
import Numeric.Units.Dimensional.Prelude hiding (exponent, zip, zipWith)
import Numeric.Units.Dimensional.Coercion (unQuantity)
import Numeric.Units.Dimensional.Presentation.Numbers
import Numeric.Units.Dimensional.Presentation.Units
import qualified Prelude as P

data PresentationQuantity d = Simple PresentationNumber (Unit 'NonMetric d ExactPi)
                            | Composite Integer (Unit 'NonMetric d ExactPi) (PresentationQuantity d)
  deriving (Generic, Typeable)

instance Show (PresentationQuantity d) where
  show (Simple x u) = show x ++ "\xA0" ++ (show $ name u)
  show (Composite n u x) = show n ++ "\xA0" ++ (show $ name u) ++ "\xA0" ++ show x

data PresentationFormat d a = PresentationFormat (PresentationUnit d) (PresentationNumberFormat a)

fixFormat :: (RealFrac a, Floating a) => PresentationFormat d a -> Quantity d a -> PresentationFormat d a
fixFormat (PresentationFormat u nf) q = PresentationFormat (fixUnit u q) nf

presentIn :: (RealFrac a, Floating a) => PresentationFormat d a -> Quantity d a -> PresentationQuantity d
presentIn f@(PresentationFormat (PrefixedUnit _ _) _) = \q -> presentIn (fixFormat f q) q
presentIn (PresentationFormat (PresentationUnit us) nf) = \q -> go (uncons prepared) (unQuantity q)
  where
    ratios xs = zipWith (P./) (cons 1 xs) xs
    factors = fmap approximateValue . ratios . fmap exactValue $ us
    prepared = zip factors us
    go ((f, u), Nothing) x = Simple (presentValueIn nf (x P.* f)) u
    go ((f, u), Just us') x = Composite n u pq
      where
        (n, x') = properFraction (x P.* f)
        pq = go (uncons us') (P.abs x')

analyze :: PresentationQuantity d -> Quantity d ExactPi
analyze (Simple x u) = value x *~ u
analyze (Composite x u q) = ipart + if x < 0 then negate fpart else fpart
  where
    ipart = fromInteger x *~ u
    fpart = analyze q

decimals :: (KnownDimension d, RealFrac a) => Int -> PresentationUnit d -> PresentationFormat d a
decimals d u = PresentationFormat u (DecimalFormat d)
