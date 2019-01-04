{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
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
import Data.Data hiding (Prefix)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Ord
import GHC.Generics hiding (Prefix)
import Numeric.Units.Dimensional.Dimensions.TermLevel (Dimension', asList, HasDimension(..))
import Numeric.Units.Dimensional.UnitNames.Atoms
import Numeric.Units.Dimensional.UnitNames.Languages
import Numeric.Units.Dimensional.Variants (Metricality(..))
import Prelude hiding ((*), (/), (^), product)
import qualified Prelude as P

-- | The name of a unit with the default choice of 'NameAtom' representation.
type UnitName m = UnitName' m NameAtom

-- | The name of a unit, parameterized by a type constructor for name atoms of a certain 'NameAtomType' and by the
-- 'Metricality' of the resulting unit name.
data UnitName' (m :: Metricality) (a :: *) where
  -- The name of the unit of dimensionless values.
  One :: UnitName' 'NonMetric a
  -- A name of an atomic unit to which metric prefixes may be applied.
  MetricAtomic :: a -> UnitName' 'Metric a
  -- A name of an atomic unit to which metric prefixes may not be applied.
  Atomic :: a -> UnitName' 'NonMetric a
  -- A name of a prefixed unit.
  Prefixed :: a -> a -> UnitName' 'NonMetric a
  -- A compound name formed from the product of two names.
  Product :: UnitName' 'NonMetric a -> UnitName' 'NonMetric a -> UnitName' 'NonMetric a
  -- A compound name formed from the quotient of two names.
  Quotient :: UnitName' 'NonMetric a -> UnitName' 'NonMetric a -> UnitName' 'NonMetric a
  -- A compound name formed by raising a unit name to an integer power.
  Power :: UnitName' 'NonMetric a -> Int -> UnitName' 'NonMetric a
  -- A compound name formed by grouping another name, which is generally compound.
  Grouped :: UnitName' 'NonMetric a -> UnitName' 'NonMetric a
  -- A weakened name formed by forgetting that it could accept a metric prefix.
  --
  -- Also available is the smart constructor `weaken` which accepts any `UnitName` as input.
  Weaken :: UnitName' 'Metric a -> UnitName' 'NonMetric a
  deriving (Typeable)

deriving instance Functor (UnitName' m)
deriving instance (Eq a) => Eq (UnitName' m a)

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

stringName :: (NameAtom -> String) -> (forall a.HasUnitName a => a -> String)
stringName f = go . fmap f . unitName
  where
    go :: UnitName' m String -> String
    go One = "1"
    go (MetricAtomic a) = a
    go (Atomic a) = a
    go (Prefixed a n) = a ++ n
    go (Product n1 n2) = go n1 ++ "\xA0" ++ go n2
    go (Quotient n1 n2) = go n1 ++ "\xA0/\xA0" ++ go (asSimple n2)
    go (Power x n) = go (asSimple x) ++ "^" ++ show n
    go (Grouped n) = "(" ++ go n ++ ")"
    go (Weaken n) = go n

class HasUnitName a where
  unitName :: a -> UnitName 'NonMetric

instance HasUnitName (UnitName m) where
  unitName = weaken

abbreviation_en :: (HasUnitName a) => a -> String
abbreviation_en = stringName $ definiteNameComponent internationalEnglishAbbreviation

name_en :: (HasUnitName a) => a -> String
name_en = stringName $ definiteNameComponent internationalEnglish

prefixAbbreviationEnglish :: Prefix -> String
prefixAbbreviationEnglish = definiteNameComponent internationalEnglishAbbreviation . prefixName

prefixNameEnglish :: Prefix -> String
prefixNameEnglish = definiteNameComponent internationalEnglish . prefixName

asAtomic :: UnitName m -> Maybe NameAtom
asAtomic (MetricAtomic a) = Just a
asAtomic (Atomic a) = Just a
asAtomic (Weaken n) = asAtomic n
asAtomic _ = Nothing

-- | A 'UnitName' is atomic if it is an atom or a weakening of an atom.
isAtomic :: UnitName m -> Bool
isAtomic = isJust . asAtomic

asMolecular :: UnitName m -> Maybe NameMolecule
asMolecular (MetricAtomic a) = Just (NameMolecule Nothing a)
asMolecular (Atomic a) = Just (NameMolecule Nothing a)
asMolecular (Prefixed p a) = Just (NameMolecule (Just p) a)
asMolecular (Weaken n) = asMolecular n
asMolecular _ = Nothing

isMolecular :: UnitName m -> Bool
isMolecular = isJust . asMolecular

-- | A 'UnitName' is simple if it is 'One', an atomic name, a prefix application to an atomic name,
-- a grouping, or a weakening of a simple name.
isSimple :: UnitName' m a -> Bool
isSimple One = True
isSimple (MetricAtomic _) = True
isSimple (Atomic _) = True
isSimple (Prefixed _ _) = True
isSimple (Grouped _) = True
isSimple (Weaken n) = isSimple n
isSimple _ = False

-- | Convert a 'UnitName' to one which is simple by grouping it if necessary.
asSimple :: UnitName' m a -> UnitName' 'NonMetric a
asSimple n | isSimple n = weaken n
           | otherwise = grouped n

-- | Convert a 'UnitName' to one in which explicit grouping expressions do not appear.
eliminateGrouping :: UnitName' m a -> UnitName' m a
eliminateGrouping (Product n1 n2) = Product (eliminateGrouping n1) (eliminateGrouping n2)
eliminateGrouping (Quotient n1 n2) = Quotient (eliminateGrouping n1) (eliminateGrouping n2)
eliminateGrouping (Power n x) = Power (eliminateGrouping n) x
eliminateGrouping (Grouped n) = n
eliminateGrouping (Weaken n) = Weaken (eliminateGrouping n)
eliminateGrouping n = n

-- | Convert a 'UnitName' to one in which inessential uses of 'One' have been eliminated.
--
-- Where 'One' appears in a 'Power' or 'Grouped' expression, that expression is eliminated.
--
-- A usage is essential if it is alone, or if it is alone in the numerator of a quotient.
eliminateOnes :: UnitName' m a -> UnitName' m a
eliminateOnes (Product n1 n2) = case (eliminateOnes n1, eliminateOnes n2) of
                                  (One, One) -> One
                                  (One, n) -> n
                                  (n, One) -> n
                                  (n1', n2') -> Product n1' n2'
eliminateOnes (Quotient n1 n2) = case eliminateOnes n2 of
                                   One -> eliminateOnes n1
                                   n2' -> Quotient (eliminateOnes n1) n2'
eliminateOnes (Power n x) = case eliminateOnes n of
                              One -> One
                              n' -> Power n' x
eliminateOnes (Grouped n) = case eliminateOnes n of
                              One -> One
                              n' -> Grouped n'
eliminateOnes (Weaken n) = Weaken (eliminateOnes n)
eliminateOnes n = n

productNormalForm :: UnitName m -> UnitName m
productNormalForm n = case n of
                        (MetricAtomic _) -> n
                        One -> n
                        Atomic _ -> n
                        Prefixed _ _ -> n
                        Product _ _ -> go n
                        Quotient _ _ -> go n
                        Power _ _ -> go n
                        Grouped _ -> go n
                        Weaken _ -> go n
  where
    go :: UnitName 'NonMetric -> UnitName 'NonMetric
    go = productOfMolecules . toMolecules

quotientNormalForm :: UnitName m -> UnitName m
quotientNormalForm n = case n of
                         (MetricAtomic _) -> n
                         One -> n
                         Atomic _ -> n
                         Prefixed _ _ -> n
                         Product _ _ -> go n
                         Quotient _ _ -> go n
                         Power _ _ -> go n
                         Grouped _ -> go n
                         Weaken _ -> go n
  where
    go :: UnitName 'NonMetric -> UnitName 'NonMetric
    go = fromMolecules . partitionMolecules . toMolecules
    partitionMolecules :: M.Map NameMolecule Int -> (M.Map NameMolecule Int, M.Map NameMolecule Int)
    partitionMolecules = M.partition (> 0) . M.filter (/= 0)
    fromMolecules :: (M.Map NameMolecule Int, M.Map NameMolecule Int) -> UnitName 'NonMetric
    fromMolecules (ns, ds) | M.null ds = productOfMolecules ns
                           | otherwise = Quotient (productOfMolecules ns) (productOfMolecules $ fmap negate ds)

productOfMolecules :: M.Map NameMolecule Int -> UnitName 'NonMetric
productOfMolecules = product . fmap build . M.toList
  where
    build :: (NameMolecule, Int) -> UnitName 'NonMetric
    build (_, 0) = One
    build ((NameMolecule (Just p) a), 1) = Prefixed p a
    build ((NameMolecule Nothing a), 1) = Atomic a
    build ((NameMolecule (Just p) a), x) = Power (Prefixed p a) x
    build ((NameMolecule Nothing a), x) = Power (Atomic a) x

toMolecules :: UnitName m -> M.Map NameMolecule Int
toMolecules One = M.empty
toMolecules (Atomic n) = M.singleton (NameMolecule Nothing n) 1
toMolecules (MetricAtomic n) = M.singleton (NameMolecule Nothing n) 1
toMolecules (Prefixed p n) = M.singleton (NameMolecule (Just p) n) 1
toMolecules (Product n1 n2) = M.unionWith (+) (toMolecules n1) (toMolecules n2)
toMolecules (Quotient n1 n2) = M.unionWith (+) (toMolecules n1) (fmap negate $ toMolecules n2)
toMolecules (Power n x) = fmap (P.* x) $ toMolecules n
toMolecules (Grouped n) = toMolecules n
toMolecules (Weaken n) = toMolecules n

-- reduce by algebraic simplifications
{-# DEPRECATED reduce "This function has strange and undocumented semantics, and will be removed in favor of more clearly documented algebraic maniupulation functions." #-}
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
applyPrefix p (MetricAtomic n) = Prefixed (prefixName p) n

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
weaken :: UnitName' m a -> UnitName' 'NonMetric a
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
strengthen :: UnitName' m a -> Maybe (UnitName' 'Metric a)
strengthen n@(MetricAtomic _) = Just n
strengthen (Weaken n) = strengthen n
strengthen _ = Nothing

-- | Convert a 'UnitName' of one 'Metricality' into a name of another metricality by
-- strengthening or weakening if neccessary. Because it may not be possible to strengthen,
-- the result is returned in a 'Maybe' wrapper.
relax :: forall m1 m2 a.(Typeable m1, Typeable m2, Typeable a) => UnitName' m1 a -> Maybe (UnitName' m2 a)
relax = go (typeRep (Proxy :: Proxy m1)) (typeRep (Proxy :: Proxy m2))
  where
    metric = typeRep (Proxy :: Proxy 'Metric)
    nonMetric = typeRep (Proxy :: Proxy 'NonMetric)
    go :: TypeRep -> TypeRep -> UnitName' m1 a -> Maybe (UnitName' m2 a)
    go p1 p2 | p1 == p2 = cast
             | (p1 == nonMetric) && (p2 == metric) = join . fmap cast . strengthen
             | (p1 == metric) && (p2 == nonMetric) = cast . weaken
             | otherwise = error "Should be unreachable. TypeRep of an unexpected Metricality encountered."

-- | Constructs a 'UnitName' by applying a grouping operation to
-- another 'UnitName', which may be useful to express precedence.
grouped :: UnitName' m a -> UnitName' 'NonMetric a
grouped = Grouped . weaken

ucumName :: (HasUnitName a) => a -> Maybe String
ucumName = ucumName' . unitName
  where
    ucumName' :: UnitName m -> Maybe String
    ucumName' One = Just "1"
    ucumName' (MetricAtomic a) = nameComponent ucum a
    ucumName' (Atomic a) = nameComponent ucum a
    ucumName' (Prefixed p n) = (++) <$> nameComponent ucum p <*> nameComponent ucum n
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
