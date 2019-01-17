{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Numeric.Units.Dimensional.UnitNames.Molecules
where

import Control.DeepSeq
import Data.Data
import Data.Group
import qualified Data.Map as M
import Data.Semigroup
import GHC.Generics
import Numeric.Units.Dimensional.UnitNames.Atoms (NameAtom)
import Numeric.Units.Dimensional.UnitNames.Prefixes
import Numeric.Units.Dimensional.Variants (Metricality)
import Prelude

type NameMolecule = NameMolecule' NameAtom

data NameMolecule' a
  = NameMolecule Metricality (Maybe (Prefix' a)) a
  deriving (Eq, Data, Typeable, Generic, NFData, Functor, Foldable, Traversable)

instance (Ord a) => Ord (NameMolecule' a) where
  compare (NameMolecule _ p1 n1) (NameMolecule _ p2 n2) = compare n1 n2 <> compare p1 p2

newtype MolecularUnitName a = MolecularUnitName (M.Map (NameMolecule' a) Int)

instance (Ord a) => Semigroup (MolecularUnitName a) where
  (MolecularUnitName m1) <> (MolecularUnitName m2) = MolecularUnitName $ M.unionWith (+) m1 m2
  stimes = stimesMonoid

instance (Ord a) => Monoid (MolecularUnitName a) where
  mempty = MolecularUnitName M.empty
  mappend = (<>)

instance (Ord a) => Group (MolecularUnitName a) where
  invert (MolecularUnitName m) = MolecularUnitName $ fmap negate m
  pow (MolecularUnitName m) x = MolecularUnitName $ fmap (* (fromIntegral x)) m

instance (Ord a) => Abelian (MolecularUnitName a)
