{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.Units.Dimensional.UnitNames.Internal
where

import Control.DeepSeq
import Control.Monad (join)
import Data.Data hiding (Prefix)
import Data.Foldable (toList)
import Data.Traversable
import Data.Group
import Data.Kind
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Ord
import Data.Semigroup (Semigroup(..), stimesMonoid)
import qualified Data.Set as S
import Data.String (IsString(..))
import Numeric.Units.Dimensional.Dimensions.TermLevel (Dimension', asList, HasDimension(..))
import Numeric.Units.Dimensional.UnitNames.Atoms
import Numeric.Units.Dimensional.UnitNames.Languages
import Numeric.Units.Dimensional.UnitNames.Molecules
import Numeric.Units.Dimensional.UnitNames.Prefixes
import Numeric.Units.Dimensional.Variants (Metricality(..))
import Prelude hiding ((*), (/), (^), product)
import qualified Prelude as P

-- | The name of a unit with the default choice of 'NameAtom' representation.
type UnitName m = UnitName' m NameAtom

-- | The name of a unit, parameterized by the type of name atoms and by the
-- 'Metricality' of the resulting unit name.
data UnitName' (m :: Metricality) (a :: Type) where
  -- | The name of the unit of dimensionless values.
  One :: UnitName' 'NonMetric a
  -- | A name of an atomic unit to which metric prefixes may be applied.
  MetricAtomic :: a -> UnitName' 'Metric a
  -- | A name of an atomic unit to which metric prefixes may not be applied.
  Atomic :: a -> UnitName' 'NonMetric a
  -- | A name of a prefixed unit.
  Prefixed :: Prefix' a -> a -> UnitName' 'NonMetric a
  -- | A compound name formed from the product of two names.
  Product :: UnitName' 'NonMetric a -> UnitName' 'NonMetric a -> UnitName' 'NonMetric a
  -- | A compound name formed from the quotient of two names.
  Quotient :: UnitName' 'NonMetric a -> UnitName' 'NonMetric a -> UnitName' 'NonMetric a
  -- | A compound name formed by raising a unit name to an integer power.
  Power :: UnitName' 'NonMetric a -> Int -> UnitName' 'NonMetric a
  -- | A compound name formed by grouping another name, which is generally compound.
  Grouped :: UnitName' 'NonMetric a -> UnitName' 'NonMetric a
  -- | A weakened name formed by forgetting that it could accept a metric prefix.
  --
  -- Also available is the smart constructor `weaken` which accepts any `UnitName` as input.
  Weaken :: UnitName' 'Metric a -> UnitName' 'NonMetric a
  deriving (Typeable)

deriving instance Functor (UnitName' m)
deriving instance Foldable (UnitName' m)
deriving instance Traversable (UnitName' m)
deriving instance (Eq a) => Eq (UnitName' m a)
#if MIN_VERSION_base(4,13,0)
deriving instance (Ord a) => Ord (UnitName' m a)
#endif

-- As it is for a GADT, this instance cannot be derived or use the generic default implementation
instance (NFData a) => NFData (UnitName' m a) where
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
  show = requiredStringName internationalEnglishAbbreviation

stringName :: (HasUnitName a, NameAtomType a ~ NameAtom) => Language 'Optional -> a -> Maybe String
stringName l = fmap foldString . traverse (nameComponent l) . applyTransform ensureSimpleDenominatorsAndPowers . weaken . name

requiredStringName :: (HasUnitName a, NameAtomType a ~ NameAtom) => Language 'Required -> a -> String
requiredStringName l = foldString . fmap (requiredNameComponent l) . applyTransform ensureSimpleDenominatorsAndPowers . weaken . name

foldString :: (IsString a, Semigroup a) => UnitName' m a -> a
foldString = foldName $ UnitNameFold {
    foldOne = fromString "1"
  , foldAtom = id
  , foldPrefix = \p n -> prefixName p <> n
  , foldProduct = \n1 n2 -> n1 <> fromString "\xA0" <> n2
  , foldQuotient = \n1 n2 -> n1 <> fromString "\xA0/\xA0" <> n2
  , foldPower = \n x -> n <> fromString "^" <> fromString (show x)
  , foldGrouped = \n -> fromString "(" <> n <> fromString ")"
  }

foldName :: UnitNameFold a b -> UnitName' m a -> b
foldName f One = foldOne f
foldName f (MetricAtomic a) = foldAtom f a
foldName f (Atomic a) = foldAtom f a
foldName f (Prefixed p n) = foldPrefix f p n
foldName f (Product n1 n2) = foldProduct f (foldName f n1) (foldName f n2)
foldName f (Quotient n1 n2) = foldQuotient f (foldName f n1) (foldName f n2)
foldName f (Power n x) = foldPower f (foldName f n) x
foldName f (Grouped n) = foldGrouped f (foldName f n)
foldName f (Weaken n) = foldName f n

data UnitNameFold a b = UnitNameFold 
  { foldOne :: b
  , foldAtom :: a -> b
  , foldPrefix :: Prefix' a -> a -> b
  , foldProduct :: b -> b -> b
  , foldQuotient :: b -> b -> b
  , foldPower :: b -> Int -> b
  , foldGrouped :: b -> b
  }

class HasUnitName a where
  type NameMetricality a :: Metricality
  type NameAtomType a :: Type
  name :: a -> UnitName' (NameMetricality a) (NameAtomType a)

instance HasUnitName (UnitName' m a) where
  type NameMetricality (UnitName' m a) = m
  type NameAtomType (UnitName' m a) = a
  name = id

asAtomic :: UnitName' m a -> Maybe a
asAtomic (MetricAtomic a) = Just a
asAtomic (Atomic a) = Just a
asAtomic (Weaken n) = asAtomic n
asAtomic _ = Nothing

-- | A 'UnitName' is atomic if it is an atom or a weakening of an atom.
isAtomic :: UnitName' m a -> Bool
isAtomic = isJust . asAtomic

asMolecular :: UnitName' m a -> Maybe (NameMolecule' a)
asMolecular (MetricAtomic a) = Just (NameMolecule Metric Nothing a)
asMolecular (Atomic a) = Just (NameMolecule NonMetric Nothing a)
asMolecular (Prefixed p a) = Just (NameMolecule NonMetric (Just p) a)
asMolecular (Weaken n) = asMolecular n
asMolecular _ = Nothing

isMolecular :: UnitName' m a -> Bool
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

evaluate :: (Group a) => UnitName' m a -> a
evaluate = foldName $ UnitNameFold {
    foldOne = mempty
  , foldAtom = id
  , foldPrefix = \p n -> prefixName p `mappend` n
  , foldProduct = mappend
  , foldQuotient = \n1 n2 -> n1 `mappend` (invert n2)
  , foldPower = pow
  , foldGrouped = id
  }

atomizePrefixes :: (Semigroup a) => UnitName' m a -> UnitName' m a
atomizePrefixes One = One
atomizePrefixes n@(MetricAtomic _) = n
atomizePrefixes n@(Atomic _) = n
atomizePrefixes (Prefixed p a) = Atomic (prefixName p <> a)
atomizePrefixes (Product n1 n2) = Product (atomizePrefixes n1) (atomizePrefixes n2)
atomizePrefixes (Quotient n1 n2) = Quotient (atomizePrefixes n1) (atomizePrefixes n2)
atomizePrefixes (Power n x) = Power (atomizePrefixes n) x
atomizePrefixes (Grouped n) = Grouped (atomizePrefixes n)
atomizePrefixes (Weaken n) = weaken $ atomizePrefixes n

evaluateMolecules :: (Group b) => (NameMolecule' a -> b) -> UnitName' m a -> b
evaluateMolecules _ One = mempty
evaluateMolecules f (MetricAtomic a) = f (NameMolecule Metric Nothing a)
evaluateMolecules f (Atomic a) = f (NameMolecule NonMetric Nothing a)
evaluateMolecules f (Prefixed p a) = f (NameMolecule NonMetric (Just p) a)
evaluateMolecules f (Product n1 n2) = evaluateMolecules f n1 `mappend` evaluateMolecules f n2
evaluateMolecules f (Quotient n1 n2) = evaluateMolecules f n1 `mappend` (invert $ evaluateMolecules f n2)
evaluateMolecules f (Power n x) = pow (evaluateMolecules f n) x
evaluateMolecules f (Grouped n) = evaluateMolecules f n
evaluateMolecules f (Weaken n) = evaluateMolecules f n

-- | Convert a 'UnitName' to one in which explicit grouping expressions do not appear.
eliminateGrouping :: UnitNameTransform' a
eliminateGrouping = Transform go
  where
    go (Product n1 n2) = Product (go n1) (go n2)
    go (Quotient n1 n2) = Quotient (go n1) (go n2)
    go (Power n x) = Power (go n) x
    go (Grouped n) = go n
    go n = n

-- | Convert a 'UnitName' to one in which inessential uses of 'One' have been eliminated.
--
-- Where 'One' appears in a 'Power' or 'Grouped' expression, that expression is eliminated.
--
-- A usage is essential if it is alone, or if it is alone in the numerator of a quotient.
eliminateOnes :: UnitNameTransform' a
eliminateOnes = Transform go
  where
    go (Product n1 n2) = case (go n1, go n2) of
                            (One, One) -> One
                            (One, n) -> n
                            (n, One) -> n
                            (n1', n2') -> Product n1' n2'
    go (Quotient n1 n2) = case go n2 of
                            One -> go n1
                            n2' -> Quotient (go n1) n2'
    go (Power n x) = case go n of
                        One -> One
                        n' -> Power n' x
    go (Grouped n) = case go n of
                        One -> One
                        n' -> Grouped n'
    go n = n

eliminateRedundantPowers :: UnitNameTransform' a
eliminateRedundantPowers = Transform go
  where
    go (Product n1 n2) = Product (go n1) (go n2)
    go (Quotient n1 n2) = Quotient (go n1) (go n2)
    go (Power One _) = One
    go (Power (Power n x1) x2) = go (Power n (x1 P.* x2))
    go n@(Power n' x) | x == 0 = One
                      | x == 1 = go n'
                      | otherwise = n
    go (Grouped n) = Grouped (go n)
    go n = n

distributePowers :: UnitNameTransform' a
distributePowers = Transform dp
  where
    dp = \case
            (Product n1 n2) -> Product (dp n1) (dp n2)
            (Quotient n1 n2) -> Quotient (dp n1) (dp n2)
            n@(Power _ _) -> go 1 n
            (Grouped n) -> Grouped (dp n)
            n -> n
    go :: Int -> UnitName' m a -> UnitName' 'NonMetric a
    go x (Product n1 n2) = Product (go x n1) (go x n2)
    go x (Quotient n1 n2) = Quotient (go x n1) (go x n2)
    go x (Power n x') = go (x P.* x') n
    go x (Grouped n) = Grouped (go x n)
    go x (Weaken n) = weaken (go x n)
    go x n = Power (weaken n) x

ensureSimpleDenominatorsAndPowers :: UnitNameTransform' a
ensureSimpleDenominatorsAndPowers = Transform go
  where
    go (Product n1 n2) = Product (go n1) (go n2)
    go (Quotient n1 n2) = Quotient (go n1) (asSimple $ go n2)
    go (Power One _) = One
    go (Power n x) = Power (asSimple $ go n) x
    go (Grouped n) = Grouped (go n)
    go n = n

productNormalForm :: (Ord a) => UnitNameTransform' a
productNormalForm = NormalForm productOfMolecules

quotientNormalForm :: (Ord a) => UnitNameTransform' a
quotientNormalForm = NormalForm $ fromMolecules . partitionMolecules
  where
    partitionMolecules :: MolecularUnitName a -> (MolecularUnitName a, MolecularUnitName a)
    partitionMolecules (MolecularUnitName m) = (\(x,y) -> (MolecularUnitName x, MolecularUnitName $ fmap negate y)) . M.partition (> 0) . M.filter (/= 0) $ m
    fromMolecules :: (MolecularUnitName a, MolecularUnitName a) -> UnitName' 'NonMetric a
    fromMolecules (ns, ds) = case productOfMolecules ds of
                               One -> productOfMolecules ns
                               ds' -> Quotient (productOfMolecules ns) ds'

productOfMolecules :: MolecularUnitName a -> UnitName' 'NonMetric a
productOfMolecules (MolecularUnitName m) = product . fmap build . M.toList $ m
  where
    build :: (NameMolecule' a, Int) -> UnitName' 'NonMetric a
    build (_, 0) = One
    build ((NameMolecule _ (Just p) a), 1) = Prefixed p a
    build ((NameMolecule Metric Nothing a), 1) = Weaken (MetricAtomic a)
    build ((NameMolecule NonMetric Nothing a), 1) = Atomic a
    build ((NameMolecule _ (Just p) a), x) = Power (Prefixed p a) x
    build ((NameMolecule Metric Nothing a), x) = Power (Weaken (MetricAtomic a)) x
    build ((NameMolecule NonMetric Nothing a), x) = Power (Atomic a) x

toMolecules :: (Ord a) => UnitName' m a -> MolecularUnitName a
toMolecules = evaluateMolecules (\m -> MolecularUnitName $ M.singleton m 1)

atoms :: (Ord a, Semigroup a) => UnitName' m a -> S.Set a
atoms = foldName $ UnitNameFold {
          foldOne = S.empty
        , foldAtom = S.singleton
        , foldPrefix = \p n -> S.singleton (prefixName p <> n)
        , foldProduct = S.union
        , foldQuotient = S.union
        , foldPower = \n _ -> n
        , foldGrouped = id
        }

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

-- | The name of the unit of dimensionless values.
nOne :: UnitName' 'NonMetric a
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

-- | Forms a 'UnitName' from a 'Metric' name by applying a metric prefix.
applyPrefix :: Prefix' a -> UnitName' 'Metric a -> UnitName' 'NonMetric a
applyPrefix p (MetricAtomic n) = Prefixed p n

{-
We will reuse the operators and function names from the Prelude.
To prevent unpleasant surprises we give operators the same fixity
as the Prelude.
-}

infixr 8  ^
infixl 7  *, /

-- | Form a 'UnitName' by taking the product of two others.
(*) :: UnitName' m1 a -> UnitName' m2 a -> UnitName' 'NonMetric a
a * b = Product (weaken a) (weaken b)

-- | Form a 'UnitName' by dividing one by another.
(/) :: UnitName' m1 a -> UnitName' m2 a -> UnitName' 'NonMetric a
n1 / n2 = Quotient (weaken n1) (weaken n2)

-- | Form a 'UnitName' by raising a name to an integer power.
(^) :: UnitName' m a -> Int -> UnitName' 'NonMetric a
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

ucumName :: (HasUnitName a, NameAtomType a ~ NameAtom) => a -> Maybe String
ucumName = fmap (foldName f) . traverse (nameComponent ucum) . applyTransform (ensureSimpleDenominatorsAndPowers <> distributePowers) . name
  where
    f = UnitNameFold {
      foldOne = "1"
    , foldAtom = id
    , foldPrefix = \p n -> prefixName p ++ n
    , foldProduct = \n1 n2 -> n1 ++ "." ++ n2
    , foldQuotient = \n1 n2 -> n1 ++ "/" ++ n2
    , foldPower = \n x -> n ++ show x
    , foldGrouped = \n -> "(" ++ n ++ ")"
    }

-- | Constructs an atomic name for a metric unit.
metricAtomic :: String -- ^ Unit name in the Unified Code for Units of Measure
             -> String -- ^ Abbreviated name in international English
             -> String -- ^ Full name in international English
             -> [(Language 'Optional, String)] -- ^ List of unit names in other 'Language's.
             -> UnitName 'Metric
metricAtomic i a f ns = MetricAtomic $ atom a f ((ucum, i):ns)

-- | Constructs an atomic name for a unit.
atomic :: String -- ^ Abbreviated name in international English
       -> String -- ^ Full name in international English
       -> [(Language 'Optional, String)] -- ^ List of unit names in other 'Language's.
       -> UnitName 'NonMetric
atomic a f ns = Atomic $ atom a f ns

-- | The type of a unit name transformation that may be associated with an operation that takes a single unit as input.
type UnitNameTransformer = (forall m a.UnitName' m a -> UnitName' 'NonMetric a)

-- | The type of a unit name transformation that may be associated with an operation that takes two units as input.
type UnitNameTransformer2 = (forall m1 m2 a.UnitName' m1 a -> UnitName' m2 a -> UnitName' 'NonMetric a)

-- | Forms the product of a list of 'UnitName's.
--
-- If you wish to form a heterogenous product of 'Metric' and 'NonMetric' units
-- you should apply 'weaken' to the 'Metric' ones.
product :: Foldable f => f (UnitName' 'NonMetric a) -> UnitName' 'NonMetric a
product = go . toList
  where
    -- This is not defined using a simple fold so that it does not complicate the product with
    -- valid but meaningless occurences of nOne.
    go :: [UnitName' 'NonMetric a] -> UnitName' 'NonMetric a
    go [] = One
    go ns = foldl1 Product ns

type UnitNameTransform = UnitNameTransform' NameAtom

-- | A 'UnitNameTransform'' is a function on 'UnitName'' which is guaranteed to preserve
-- normal forms.
data UnitNameTransform' a where
  Identity :: UnitNameTransform' a
  Compose :: UnitNameTransform' a -> UnitNameTransform' a -> UnitNameTransform' a
  Transform :: (UnitName' 'NonMetric a -> UnitName' 'NonMetric a) -> UnitNameTransform' a
  NormalForm :: (Ord a) => (MolecularUnitName a -> UnitName' 'NonMetric a) -> UnitNameTransform' a

-- | 'UnitNameTransformer's form a semigroup under composition.
instance Semigroup (UnitNameTransform' a) where
  t <> Identity = t
  Identity <> t = t
  n@(NormalForm _) <> _ = n
  t2 <> t1 = Compose t2 t1
  stimes = stimesMonoid

-- | 'UnitNameTransformer's form a monoid under composition.
instance Monoid (UnitNameTransform' a) where
  mempty = Identity
  mappend = (Data.Semigroup.<>)

-- | Converts a 'UnitNameTransform' to a function on 'UnitName''s.
applyTransform :: UnitNameTransform' a -> UnitName' m a -> UnitName' m a
applyTransform Identity = id
applyTransform (Compose t2 t1) = applyTransform t2 . applyTransform t1
applyTransform (NormalForm f) = applyTransform (Transform (f . toMolecules))
applyTransform (Transform f) = \n -> case n of
                                  (MetricAtomic _) -> n
                                  One -> n
                                  Atomic _ -> n
                                  Prefixed _ _ -> n
                                  Product _ _ -> f n
                                  Quotient _ _ -> f n
                                  Power _ _ -> f n
                                  Grouped _ -> f n
                                  Weaken _ -> f n
