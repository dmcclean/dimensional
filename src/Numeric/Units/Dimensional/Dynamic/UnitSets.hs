{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Units.Dimensional.Dynamic.UnitSets
(
  UnitSet
, UnitSet'
, null
, singleton
, singleton'
, fromList
, applyLanguages
, applyPrefixes
, mapNames
, mapNames'
, mapUnitSet
, lookup
, isAmbiguous
, partitionByAmbiguity
, resolve
, units
)
where

import Numeric.Units.Dimensional.UnitNames (name, NameAtom, Metricality(..), Prefix, asAtomic, strengthen, nameComponent, prefixName)
import Numeric.Units.Dimensional.UnitNames.Internal (UnitName'(..), foldName, UnitNameFold(..))
import Numeric.Units.Dimensional.UnitNames.Languages
import Numeric.Units.Dimensional.UnitNames.Molecules (NameMolecule, NameMolecule'(..))
import Numeric.Units.Dimensional.Dynamic (AnyUnit, applyPrefix, demoteUnit')
import Numeric.Units.Dimensional (one)
import qualified Numeric.Units.Dimensional.Dynamic as Dyn
import Control.Applicative (liftA2)
import Control.Monad (join)
import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup(..))
import qualified Data.Map as M
import Prelude hiding (lookup, null)

type UnitSet = UnitSet' NameAtom

newtype UnitSet' n = UnitSet (M.Map n [AnyUnit])
  deriving Show

instance (Ord n) => Semigroup (UnitSet' n) where
  (UnitSet u1) <> (UnitSet u2) = UnitSet (M.unionWith (++) u1 u2)

instance (Ord n) => Monoid (UnitSet' n) where
  mempty = UnitSet M.empty
  mappend = (Data.Semigroup.<>)

null :: UnitSet' a -> Bool
null (UnitSet us) = M.null us

singleton :: a -> AnyUnit -> UnitSet' a
singleton n u = UnitSet $ M.singleton n [u]

singleton' :: AnyUnit -> UnitSet
singleton' u = case asAtomic (name u) of
                 Just n -> singleton n u
                 Nothing -> mempty

fromList :: [AnyUnit] -> UnitSet
fromList = foldMap singleton'

applyLanguages :: [Language 'Optional] -> UnitSet' NameMolecule -> UnitSet' String
applyLanguages ls = mapUnitSet go
  where
    go :: NameMolecule -> AnyUnit -> UnitSet' String
    go n u = foldMap (inLanguage n u) ls
    inLanguage :: NameMolecule -> AnyUnit -> Language 'Optional -> UnitSet' String
    inLanguage n u l = case traverse (nameComponent l) n of
                          Just n' -> singleton (fold n') u
                          Nothing -> mempty

-- TODO: take a PrefixSet instead of a [Prefix]?
-- TODO: generalize
applyPrefixes :: [Prefix] -> UnitSet -> UnitSet' NameMolecule
applyPrefixes ps = mapUnitSet go
  where
    go :: NameAtom -> AnyUnit -> UnitSet' NameMolecule
    go n u = case strengthen (name u) of
               Just _ -> applyAllPrefixes n u
               Nothing -> singleton (NameMolecule NonMetric Nothing n) u
    applyAllPrefixes :: NameAtom -> AnyUnit -> UnitSet' NameMolecule
    applyAllPrefixes n u = foldMap (applyOnePrefix n u) ps <> singleton (NameMolecule Metric Nothing n) u
    applyOnePrefix :: NameAtom -> AnyUnit -> Prefix -> UnitSet' NameMolecule
    applyOnePrefix n u p = case applyPrefix p u of
                             Nothing -> mempty
                             Just u' -> singleton (NameMolecule NonMetric (Just p) n) u'

mapNames :: (Ord b) => (a -> b) -> UnitSet' a -> UnitSet' b
mapNames f = mapNames' ((:[]) . f)

mapNames' :: forall a b.(Ord b) => (a -> [b]) -> UnitSet' a -> UnitSet' b
mapNames' f = mapUnitSet go
  where
    go :: a -> AnyUnit -> UnitSet' b
    go n u = foldMap (\n' -> singleton n' u) $ f n

mapUnitSet :: (Ord b) => (a -> AnyUnit -> UnitSet' b) -> UnitSet' a -> UnitSet' b
mapUnitSet f = foldMap (uncurry f) . toList
  where
    toList :: UnitSet' a -> [(a, AnyUnit)]
    toList (UnitSet us) = join $ fmap (traverse id) $ M.toList us

units :: UnitSet' a -> [AnyUnit]
units (UnitSet us) = join $ M.elems us

lookup :: (Ord a) => a -> UnitSet' a -> [AnyUnit]
lookup n (UnitSet us) = fromMaybe [] $ M.lookup n us

isAmbiguous :: UnitSet' n -> Bool
isAmbiguous (UnitSet u) = any (\us -> length us > 1) u

partitionByAmbiguity :: UnitSet' n -> (UnitSet' n, UnitSet' n)
partitionByAmbiguity (UnitSet u) = let (unamb, amb) = M.partition (\us -> length us <= 1) u
                                    in (UnitSet unamb, UnitSet amb)

foldAnyUnit :: (Semigroup a, Applicative f) => (a -> f AnyUnit) -> UnitName' m a -> f AnyUnit
foldAnyUnit f = foldName $ UnitNameFold {
    foldOne = pure $ demoteUnit' one
  , foldAtom = f
  , foldPrefix = \p n -> f (prefixName p <> n)
  , foldProduct = liftA2 (Dyn.*)
  , foldQuotient = liftA2 (Dyn./)
  , foldPower = \u x -> (Dyn.^ x) <$> u
  , foldGrouped = fmap Dyn.grouped
  }

resolve :: (Ord a, Semigroup a) => UnitSet' a -> UnitName' m a -> [AnyUnit]
resolve us = foldAnyUnit (\n -> lookup n us)
