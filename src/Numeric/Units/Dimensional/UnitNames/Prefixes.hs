{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Numeric.Units.Dimensional.UnitNames.Prefixes
(
  Prefix
, Prefix'(..)
, siPrefixes
, yocto, zepto, atto, femto, pico, nano, micro, milli, centi, deci, deka, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta
)
where

import Prelude
import Control.DeepSeq
import Data.Data hiding (Prefix)
import Data.Ord (comparing)
import GHC.Generics hiding (Prefix)
import Numeric.Units.Dimensional.UnitNames.Atoms
import Numeric.Units.Dimensional.UnitNames.Languages (ucum, siunitx)

type Prefix = Prefix' NameAtom

data Prefix' a = Prefix
                  {
                    -- | The name of a metric prefix.
                    prefixName :: a,
                    -- | The scale factor denoted by a metric prefix.
                    scaleExponent :: Int
                  }
  deriving (Eq, Data, Typeable, Generic, Functor, Foldable, Traversable)

instance (NFData a) => NFData (Prefix' a)

instance (Ord a) => Ord (Prefix' a) where
  compare = comparing scaleExponent

deka, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta :: Prefix
deka  = prefix "da" "da" "deka" 1
hecto = prefix "h" "h" "hecto"  2
kilo  = prefix "k" "k" "kilo"   3
mega  = prefix "M" "M" "mega"   6
giga  = prefix "G" "G" "giga"   9
tera  = prefix "T" "T" "tera"   12
peta  = prefix "P" "P" "peta"   15
exa   = prefix "E" "E" "exa"    18
zetta = prefix "Z" "Z" "zetta"  21
yotta = prefix "Y" "Y" "yotta"  24
deci, centi, milli, micro, nano, pico, femto, atto, zepto, yocto :: Prefix
deci  = prefix "d" "d" "deci"   $ -1
centi = prefix "c" "c" "centi"  $ -2
milli = prefix "m" "m" "milli"  $ -3
micro = prefix "u" "μ" "micro"  $ -6
nano  = prefix "n" "n" "nano"   $ -9
pico  = prefix "p" "p" "pico"   $ -12
femto = prefix "f" "f" "femto"  $ -15
atto  = prefix "a" "a" "atto"   $ -18
zepto = prefix "z" "z" "zepto"  $ -21
yocto = prefix "y" "y" "yocto"  $ -24

prefix :: String -> String -> String -> Int -> Prefix
prefix i a f = Prefix n
  where
    n = atom a f [(ucum, i), (siunitx, '\\' : f)]

-- | A list of all 'Prefix'es defined by the SI.
siPrefixes :: [Prefix]
siPrefixes = [yocto, zepto, atto, femto, pico, nano, micro, milli, centi, deci, deka, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta]
