{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Numeric.Units.Dimensional.UnitNames.Internal
where

import Control.Applicative
import Control.DeepSeq
import Control.Monad (join)
import qualified Data.Char as C
import Data.Coerce (coerce)
import Data.Data hiding (Prefix)
import Data.Foldable (toList)
import qualified Data.Map.Strict as M
import Data.Ord
import GHC.Generics hiding (Prefix)
import Numeric.Units.Dimensional.Dimensions.TermLevel (Dimension', asList, HasDimension(..))
import Numeric.Units.Dimensional.Variants (Metricality(..))
import Prelude hiding ((*), (/), (^), product)
import qualified Prelude as P

-- | The name of a unit.
data UnitName (m :: Metricality) where
  -- The name of the unit of dimensionless values.
  One :: UnitName 'NonMetric
  -- A name of an atomic unit to which metric prefixes may be applied.
  MetricAtomic :: NameAtom ('UnitAtom 'Metric) -> UnitName 'Metric
  -- A name of an atomic unit to which metric prefixes may not be applied.
  Atomic :: NameAtom ('UnitAtom 'NonMetric) -> UnitName 'NonMetric
  -- A name of a prefixed unit.
  Prefixed :: PrefixName -> UnitName 'Metric -> UnitName 'NonMetric
  -- A compound name formed from the product of two names.
  Product :: UnitName 'NonMetric -> UnitName 'NonMetric -> UnitName 'NonMetric
  -- A compound name formed from the quotient of two names.
  Quotient :: UnitName 'NonMetric -> UnitName 'NonMetric -> UnitName 'NonMetric
  -- A compound name formed by raising a unit name to an integer power.
  Power :: UnitName 'NonMetric -> Int -> UnitName 'NonMetric
  -- A compound name formed by grouping another name, which is generally compound.
  Grouped :: UnitName 'NonMetric -> UnitName 'NonMetric
  -- A weakened name formed by forgetting that it could accept a metric prefix.
  --
  -- Also available is the smart constructor `weaken` which accepts any `UnitName` as input.
  Weaken :: UnitName 'Metric -> UnitName 'NonMetric
  deriving (Typeable)

deriving instance Eq (UnitName m)

-- As it is for a GADT, this instance cannot be derived or use the generic default implementation
instance NFData (UnitName m) where
  rnf n = case n of
    One -> ()
    MetricAtomic a -> rnf a
    Atomic a -> rnf a
    Prefixed p n' -> rnf p `seq` rnf n'
    Product n1 n2 -> rnf n1 `seq` rnf n2
    Quotient n1 n2 -> rnf n1 `seq` rnf n2
    Power n' e -> rnf n' `seq` rnf e
    Grouped n' -> rnf n'
    Weaken n' -> rnf n'

-- | `UnitName`s are shown with non-breaking spaces.
instance Show (UnitName m) where
  show = abbreviation_en

stringName :: (forall t.NameAtom t -> String) -> (forall a.HasUnitName a => a -> String)
stringName f = go . unitName
  where
    go :: UnitName m -> String
    go One = "1"
    go (MetricAtomic a) = f a
    go (Atomic a) = f a
    go (Prefixed a n) = f a ++ show n
    go (Product n1 n2) = go n1 ++ "\xA0" ++ go n2
    go (Quotient n1 n2) = go n1 ++ "\xA0/\xA0" ++ go n2 -- TODO: handle the case where grouping is required
    go (Power x n) = go x ++ "^" ++ show n -- TODO: handle the case where grouping is required
    go (Grouped n) = "(" ++ go n ++ ")"
    go (Weaken n) = go n

class HasUnitName a where
  unitName :: a -> UnitName 'NonMetric

instance HasUnitName (UnitName m) where
  unitName = weaken

instance HasUnitName (NameAtom ('UnitAtom 'Metric)) where
  unitName = Weaken . MetricAtomic

instance HasUnitName (NameAtom ('UnitAtom 'NonMetric)) where
  unitName = Atomic

abbreviation_en :: (HasUnitName a) => a -> String
abbreviation_en = stringName $ definiteNameComponent internationalEnglishAbbreviation

name_en :: (HasUnitName a) => a -> String
name_en = stringName $ definiteNameComponent internationalEnglish

prefixAbbreviationEnglish :: Prefix -> String
prefixAbbreviationEnglish = definiteNameComponent internationalEnglishAbbreviation . prefixName

prefixNameEnglish :: Prefix -> String
prefixNameEnglish = definiteNameComponent internationalEnglish . prefixName

definiteNameComponent :: Language -> NameAtom m -> String
definiteNameComponent l (NameAtom m) = m M.! l

nameComponent :: Language -> NameAtom m -> Maybe String
nameComponent l (NameAtom m) = M.lookup l m

asAtomic :: UnitName m -> Maybe (NameAtom ('UnitAtom m))
asAtomic (MetricAtomic a) = Just a
asAtomic (Atomic a) = Just a
asAtomic (Weaken n) = coerce <$> asAtomic n
asAtomic _ = Nothing

isAtomic :: UnitName m -> Bool
isAtomic One = True
isAtomic (MetricAtomic _) = True
isAtomic (Atomic _) = True
isAtomic _ = False

-- reduce by algebraic simplifications
reduce :: UnitName m -> UnitName m
reduce One = One
reduce n@(MetricAtomic _) = n
reduce n@(Atomic _) = n
reduce n@(Prefixed _ _) = n
reduce (Product n1 n2) = reduce' (reduce n1 * reduce n2)
reduce (Quotient n1 n2) = reduce' (reduce n1 * reduce n2)
reduce (Power n x) = reduce' (reduce n ^ x)
reduce (Grouped n) = reduce' (Grouped (reduce n))
reduce (Weaken n) = reduce' (Weaken (reduce n))

-- reduce, knowing that subterms are already in reduced form
reduce' :: UnitName m -> UnitName m
reduce' (Product One n) = reduce' n
reduce' (Product n One) = reduce' n
reduce' (Power (Power n x1) x2) = reduce (n ^ (x1 P.* x2))
reduce' (Power (Grouped (Power n x1)) x2) = reduce (n ^ (x1 P.* x2))
reduce' (Power _ 0) = One
reduce' (Power n 1) = reduce' n
reduce' (Grouped n) = reduce' n
reduce' n@(Weaken (MetricAtomic _)) = n
reduce' n = n

data NameAtomType = UnitAtom Metricality
                  | PrefixAtom
  deriving (Eq, Ord, Data, Typeable, Generic)

instance NFData NameAtomType where -- instance is derived from Generic instance

-- | The name of a metric prefix.
type PrefixName = NameAtom 'PrefixAtom

data Prefix = Prefix
              {
                -- | The name of a metric prefix.
                prefixName :: PrefixName,
                -- | The scale factor denoted by a metric prefix.
                scaleExponent :: Int
              }
  deriving (Eq, Data, Typeable, Generic)

instance Ord Prefix where
  compare = comparing scaleExponent

instance NFData Prefix where -- instance is derived from Generic instance

-- | The name of the unit of dimensionless values.
nOne :: UnitName 'NonMetric
nOne = One

nMeter :: UnitName 'Metric
nMeter = metricAtomic "m" "m" "metre" [(siunitx, "\\metre"), (usEnglish, "meter")]

nGram :: UnitName 'Metric
nGram = metricAtomic "g" "g" "gram" [(siunitx, "\\gram")]

nKilogram :: UnitName 'NonMetric
nKilogram = applyPrefix kilo nGram

nSecond :: UnitName 'Metric
nSecond = metricAtomic "s" "s" "second" [(siunitx, "\\second")]

nAmpere :: UnitName 'Metric
nAmpere = metricAtomic "A" "A" "Ampere" [(siunitx, "\\ampere")]

nKelvin :: UnitName 'Metric
nKelvin = metricAtomic "K" "K" "Kelvin" [(siunitx, "\\kelvin")]

nMole :: UnitName 'Metric
nMole = metricAtomic "mol" "mol" "mole" [(siunitx, "\\mole")]

nCandela :: UnitName 'Metric
nCandela = metricAtomic "cd" "cd" "candela" [(siunitx, "\\candela")]

-- | The name of the base unit associated with a specified dimension.
baseUnitName :: Dimension' -> UnitName 'NonMetric
baseUnitName d = let powers = asList $ dimension d
                  in reduce . product $ zipWith (^) baseUnitNames powers

baseUnitNames :: [UnitName 'NonMetric]
baseUnitNames = [weaken nMeter, nKilogram, weaken nSecond, weaken nAmpere, weaken nKelvin, weaken nMole, weaken nCandela]

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

-- | A list of all 'Prefix'es defined by the SI.
siPrefixes :: [Prefix]
siPrefixes = [yocto, zepto, atto, femto, pico, nano, micro, milli, centi, deci, deka, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta]

-- | Forms a 'UnitName' from a 'Metric' name by applying a metric prefix.
applyPrefix :: Prefix -> UnitName 'Metric -> UnitName 'NonMetric
applyPrefix = Prefixed . prefixName

{-
We will reuse the operators and function names from the Prelude.
To prevent unpleasant surprises we give operators the same fixity
as the Prelude.
-}

infixr 8  ^
infixl 7  *, /

-- | Form a 'UnitName' by taking the product of two others.
(*) :: UnitName m1 -> UnitName m2 -> UnitName 'NonMetric
a * b = Product (weaken a) (weaken b)

-- | Form a 'UnitName' by dividing one by another.
(/) :: UnitName m1 -> UnitName m2 -> UnitName 'NonMetric
n1 / n2 = Quotient (weaken n1) (weaken n2)

-- | Form a 'UnitName' by raising a name to an integer power.
(^) :: UnitName m -> Int -> UnitName 'NonMetric
x ^ n = Power (weaken x) n

-- | Convert a 'UnitName' which may or may not be 'Metric' to one
-- which is certainly 'NonMetric'.
weaken :: UnitName m -> UnitName 'NonMetric
weaken n@(MetricAtomic _) = Weaken n -- we really only need this one case and a catchall, but the typechecker can't see it
weaken n@One = n
weaken n@(Atomic _) = n
weaken n@(Prefixed _ _) = n
weaken n@(Product _ _) = n
weaken n@(Quotient _ _) = n
weaken n@(Power _ _) = n
weaken n@(Grouped _) = n
weaken n@(Weaken _) = n

-- | Attempt to convert a 'UnitName' which may or may not be 'Metric' to one
-- which is certainly 'Metric'.
strengthen :: UnitName m -> Maybe (UnitName 'Metric)
strengthen n@(MetricAtomic _) = Just n
strengthen (Weaken n) = strengthen n
strengthen _ = Nothing

-- | Convert a 'UnitName' of one 'Metricality' into a name of another metricality by
-- strengthening or weakening if neccessary. Because it may not be possible to strengthen,
-- the result is returned in a 'Maybe' wrapper.
relax :: forall m1 m2.(Typeable m1, Typeable m2) => UnitName m1 -> Maybe (UnitName m2)
relax = go (typeRep (Proxy :: Proxy m1)) (typeRep (Proxy :: Proxy m2))
  where
    metric = typeRep (Proxy :: Proxy 'Metric)
    nonMetric = typeRep (Proxy :: Proxy 'NonMetric)
    go :: TypeRep -> TypeRep -> UnitName m1 -> Maybe (UnitName m2)
    go p1 p2 | p1 == p2 = cast
             | (p1 == nonMetric) && (p2 == metric) = join . fmap gcast . strengthen
             | (p1 == metric) && (p2 == nonMetric) = cast . weaken
             | otherwise = error "Should be unreachable. TypeRep of an unexpected Metricality encountered."

-- | Constructs a 'UnitName' by applying a grouping operation to
-- another 'UnitName', which may be useful to express precedence.
grouped :: UnitName m -> UnitName 'NonMetric
grouped = Grouped . weaken

-- | An IETF language tag.
type Language = String

-- | The language of unit names standardized by the Unified Code for Units of Measure.
--
-- See <http://unitsofmeasure.org/ here> for further information.
ucum :: Language
ucum = "x-ucum"

-- | The language of unit names used by the siunitx LaTeX package.
--
-- See <https://www.ctan.org/pkg/siunitx here> for further information.
siunitx :: Language
siunitx = "x-siunitx"

-- | The English language.
internationalEnglish :: Language
internationalEnglish = "en"

-- | The English language, restricted to the ASCII character set.
internationalEnglishAscii :: Language
internationalEnglishAscii = "en-x-ascii"

-- | The English language, as used in the United States.
usEnglish :: Language
usEnglish = "en-US"

-- | An abbreviation in the English language.
internationalEnglishAbbreviation :: Language
internationalEnglishAbbreviation = "en-x-abbrev"

-- | An abbreviation in the English language, restricted to the ASCII character set.
internationalEnglishAsciiAbbreviation :: Language
internationalEnglishAsciiAbbreviation = "en-x-abbrev-ascii"

isValidAscii :: String -> Bool
isValidAscii = all (\c -> C.isAscii c && C.isPrint c)

-- | Represents the name of an atomic unit or prefix.
newtype NameAtom (m :: NameAtomType)
  = NameAtom (M.Map Language String) -- It's an invariant that internationalEnglish and internationalEnglishAbbreviation must appear as keys in the map. If the 'NameAtomType' is a prefix or metric, it must also contain 'ucum' as a key.
  deriving (Eq, Data, Typeable, Generic)

instance NFData (NameAtom m) where -- instance is derived from Generic instance

ucumName :: (HasUnitName a) => a -> Maybe String
ucumName = ucumName' . unitName
  where
    ucumName' :: UnitName m -> Maybe String
    ucumName' One = Just "1"
    ucumName' (MetricAtomic a) = nameComponent ucum a
    ucumName' (Atomic a) = nameComponent ucum a
    ucumName' (Prefixed p n) = (++) <$> nameComponent ucum p <*> ucumName' n
    ucumName' (Product n1 n2) = do
                                  n1' <- ucumName' n1
                                  n2' <- ucumName' n2
                                  return $ n1' ++ "." ++ n2'
    -- TODO: does one of these subexpressions require a grouping if it is itself a quotient? seems like it must
    -- we did it at construction time, but if we are going to expose the constructors then we need to do it again.
    ucumName' (Quotient n1 n2) = do
                                   n1' <- ucumName' n1
                                   n2' <- ucumName' n2
                                   return $ n1' ++ "/" ++ n2'
    -- TODO #109: note in this case that the UCUM is changing their grammar to not accept exponents after
    -- as a result it will become necessary to distribute the exponentiation over the items in the base name
    -- prior to generating the UCUM name
    ucumName' (Power n x) = do
                              n' <- ucumName' n
                              return $ n' ++ show x
    ucumName' (Grouped n) = (\x -> "(" ++ x ++ ")") <$> ucumName' n
    ucumName' (Weaken n) = ucumName' n

prefix :: String -> String -> String -> Int -> Prefix
prefix i a f = Prefix n
  where
    n = atom a f [(ucum, i), (siunitx, '\\' : f)]

-- | Constructs an atomic name for a metric unit.
metricAtomic :: String -- ^ Unit name in the Unified Code for Units of Measure
             -> String -- ^ Abbreviated name in international English
             -> String -- ^ Full name in international English
             -> [(Language, String)] -- ^ List of unit names in other 'Language's.
             -> UnitName 'Metric
metricAtomic i a f ns = MetricAtomic $ atom a f ((ucum, i):ns)

-- | Constructs an atomic name for a unit.
atomic :: String -- ^ Abbreviated name in international English
       -> String -- ^ Full name in international English
       -> [(Language, String)] -- ^ List of unit names in other 'Language's.
       -> UnitName 'NonMetric
atomic a f ns = Atomic $ atom a f ns

-- | Constructs a 'NameAtom' of some 'NameAtomType'.
atom :: String -- ^ Abbreviated name in international English
     -> String -- ^ Full name in international English
     -> [(Language, String)] -- ^ List of names in other 'Language's.
     -> NameAtom t
atom a f ns = NameAtom $ M.union (M.fromList ns) ascii'
  where
    ascii' = if (isValidAscii f) then M.insert internationalEnglishAscii f ascii else ascii
    ascii = if (isValidAscii a) then M.insert internationalEnglishAsciiAbbreviation a basic else basic
    basic = M.fromList [(internationalEnglishAbbreviation, a), (internationalEnglish, f)]

-- | The type of a unit name transformation that may be associated with an operation that takes a single unit as input.
type UnitNameTransformer = (forall m.UnitName m -> UnitName 'NonMetric)

-- | The type of a unit name transformation that may be associated with an operation that takes two units as input.
type UnitNameTransformer2 = (forall m1 m2.UnitName m1 -> UnitName m2 -> UnitName 'NonMetric)

-- | Forms the product of a list of 'UnitName's.
--
-- If you wish to form a heterogenous product of 'Metric' and 'NonMetric' units
-- you should apply 'weaken' to the 'Metric' ones.
product :: Foldable f => f (UnitName 'NonMetric) -> UnitName 'NonMetric
product = go . toList
  where
    -- This is not defined using a simple fold so that it does not complicate the product with
    -- valid but meaningless occurences of nOne.
    go :: [UnitName 'NonMetric] -> UnitName 'NonMetric
    go [] = nOne
    go [n] = n
    go (n : ns) = n * go ns
