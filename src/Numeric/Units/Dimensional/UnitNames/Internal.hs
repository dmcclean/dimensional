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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.Units.Dimensional.UnitNames.Internal
where

import Control.Applicative
import Control.DeepSeq
import Control.Monad (join)
import Data.Data hiding (Prefix)
import Data.Foldable (toList)
import Data.Group
import Data.Kind
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Ord
import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import Numeric.Units.Dimensional.Dimensions.TermLevel (Dimension', asList, HasDimension(..))
import Numeric.Units.Dimensional.UnitNames.Atoms
import Numeric.Units.Dimensional.UnitNames.Languages
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
  Prefixed :: a -> a -> UnitName' 'NonMetric a
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
deriving instance (Eq a) => Eq (UnitName' m a)

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
  show = abbreviation_en

stringName :: (HasUnitName a) => (NameAtomType a -> String) -> a -> String
stringName f = foldString . fmap f . ensureSimpleDenominatorsAndPowers . weaken . unitName

foldString :: (IsString a, Semigroup a) => UnitName' m a -> a
foldString = foldName $ UnitNameFold {
    foldOne = fromString "1"
  , foldPrefix = (<>)
  , foldProduct = \n1 n2 -> n1 <> fromString "\xA0" <> n2
  , foldQuotient = \n1 n2 -> n1 <> fromString "\xA0/\xA0" <> n2
  , foldPower = \n x -> n <> fromString "^" <> fromString (show x)
  , foldGrouped = \n -> fromString "(" <> n <> fromString ")"
  }

foldName :: UnitNameFold a -> UnitName' m a -> a
foldName f One = foldOne f
foldName _ (MetricAtomic a) = a
foldName _ (Atomic a) = a
foldName f (Prefixed p n) = foldPrefix f p n
foldName f (Product n1 n2) = foldProduct f (foldName f n1) (foldName f n2)
foldName f (Quotient n1 n2) = foldQuotient f (foldName f n1) (foldName f n2)
foldName f (Power n x) = foldPower f (foldName f n) x
foldName f (Grouped n) = foldGrouped f (foldName f n)
foldName f (Weaken n) = foldName f n

data UnitNameFold a = UnitNameFold 
  { foldOne :: a
  , foldPrefix :: a -> a -> a
  , foldProduct :: a -> a -> a
  , foldQuotient :: a -> a -> a
  , foldPower :: a -> Int -> a
  , foldGrouped :: a -> a
  }

class HasUnitName a where
  type NameMetricality a :: Metricality
  type NameAtomType a :: Type
  unitName :: a -> UnitName' (NameMetricality a) (NameAtomType a)

instance HasUnitName (UnitName' m a) where
  type NameMetricality (UnitName' m a) = m
  type NameAtomType (UnitName' m a) = a
  unitName = id

abbreviation_en :: (HasUnitName a, NameAtomType a ~ NameAtom) => a -> String
abbreviation_en = stringName $ definiteNameComponent internationalEnglishAbbreviation

name_en :: (HasUnitName a, NameAtomType a ~ NameAtom) => a -> String
name_en = stringName $ definiteNameComponent internationalEnglish

prefixAbbreviationEnglish :: Prefix -> String
prefixAbbreviationEnglish = definiteNameComponent internationalEnglishAbbreviation . prefixName

prefixNameEnglish :: Prefix -> String
prefixNameEnglish = definiteNameComponent internationalEnglish . prefixName

asAtomic :: UnitName' m a -> Maybe a
asAtomic (MetricAtomic a) = Just a
asAtomic (Atomic a) = Just a
asAtomic (Weaken n) = asAtomic n
asAtomic _ = Nothing

-- | A 'UnitName' is atomic if it is an atom or a weakening of an atom.
isAtomic :: UnitName' m a -> Bool
isAtomic = isJust . asAtomic

asMolecular :: UnitName' m a -> Maybe (NameMolecule a)
asMolecular (MetricAtomic a) = Just (NameMolecule Nothing a)
asMolecular (Atomic a) = Just (NameMolecule Nothing a)
asMolecular (Prefixed p a) = Just (NameMolecule (Just p) a)
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
  , foldPrefix = mappend
  , foldProduct = mappend
  , foldQuotient = \n1 n2 -> n1 `mappend` (invert n2)
  , foldPower = pow
  , foldGrouped = id
  }

evaluateMolecules :: (Group b) => (NameMolecule a -> b) -> UnitName' m a -> b
evaluateMolecules _ One = mempty
evaluateMolecules f (MetricAtomic a) = f (NameMolecule Nothing a)
evaluateMolecules f (Atomic a) = f (NameMolecule Nothing a)
evaluateMolecules f (Prefixed p a) = f (NameMolecule (Just p) a)
evaluateMolecules f (Product n1 n2) = evaluateMolecules f n1 `mappend` evaluateMolecules f n2
evaluateMolecules f (Quotient n1 n2) = evaluateMolecules f n1 `mappend` (invert $ evaluateMolecules f n2)
evaluateMolecules f (Power n x) = pow (evaluateMolecules f n) x
evaluateMolecules f (Grouped n) = evaluateMolecules f n
evaluateMolecules f (Weaken n) = evaluateMolecules f n

-- | Convert a 'UnitName' to one in which explicit grouping expressions do not appear.
eliminateGrouping :: UnitName' m a -> UnitName' m a
eliminateGrouping (Product n1 n2) = Product (eliminateGrouping n1) (eliminateGrouping n2)
eliminateGrouping (Quotient n1 n2) = Quotient (eliminateGrouping n1) (eliminateGrouping n2)
eliminateGrouping (Power n x) = Power (eliminateGrouping n) x
eliminateGrouping (Grouped n) = eliminateGrouping n
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

eliminateRedundantPowers :: UnitName' m a -> UnitName' m a
eliminateRedundantPowers (Product n1 n2) = Product (eliminateRedundantPowers n1) (eliminateRedundantPowers n2)
eliminateRedundantPowers (Quotient n1 n2) = Quotient (eliminateRedundantPowers n1) (eliminateRedundantPowers n2)
eliminateRedundantPowers (Power One _) = One
eliminateRedundantPowers (Power (Power n x1) x2) = eliminateRedundantPowers (Power n (x1 P.* x2))
eliminateRedundantPowers n@(Power n' x) | x == 0 = One
                                        | x == 1 = eliminateRedundantPowers n'
                                        | otherwise = n
eliminateRedundantPowers (Grouped n) = Grouped (eliminateRedundantPowers n)
eliminateRedundantPowers (Weaken n) = Weaken (eliminateRedundantPowers n)
eliminateRedundantPowers n = n

distributePowers :: UnitName' m a -> UnitName' m a
distributePowers = \case
                      (Product n1 n2) -> Product (distributePowers n1) (distributePowers n2)
                      (Quotient n1 n2) -> Quotient (distributePowers n1) (distributePowers n2)
                      n@(Power _ _) -> go 1 n
                      (Grouped n) -> Grouped (distributePowers n)
                      (Weaken n) -> Weaken (distributePowers n)
                      One -> One
                      n@(Atomic _) -> n
                      n@(MetricAtomic _) -> n
                      n@(Prefixed _ _) -> n
  where
    go :: Int -> UnitName' m a -> UnitName' 'NonMetric a
    go x (Product n1 n2) = Product (go x n1) (go x n2)
    go x (Quotient n1 n2) = Quotient (go x n1) (go x n2)
    go x (Power n x') = go (x P.* x') n
    go x (Grouped n) = Grouped (go x n)
    go x (Weaken n) = weaken (go x n)
    go x n = Power (weaken n) x

ensureSimpleDenominatorsAndPowers :: UnitName' m a -> UnitName' m a
ensureSimpleDenominatorsAndPowers (Product n1 n2) = Product (ensureSimpleDenominatorsAndPowers n1) (ensureSimpleDenominatorsAndPowers n2)
ensureSimpleDenominatorsAndPowers (Quotient n1 n2) = Quotient (ensureSimpleDenominatorsAndPowers n1) (asSimple $ ensureSimpleDenominatorsAndPowers n2)
ensureSimpleDenominatorsAndPowers (Power One _) = One
ensureSimpleDenominatorsAndPowers (Power n x) = Power (asSimple $ ensureSimpleDenominatorsAndPowers n) x
ensureSimpleDenominatorsAndPowers (Grouped n) = Grouped (ensureSimpleDenominatorsAndPowers n)
ensureSimpleDenominatorsAndPowers (Weaken n) = Weaken (ensureSimpleDenominatorsAndPowers n)
ensureSimpleDenominatorsAndPowers n = n

newtype MolecularUnitName a = MolecularUnitName (M.Map (NameMolecule a) Int)

instance (Ord a) => Semigroup (MolecularUnitName a) where
  (MolecularUnitName m1) <> (MolecularUnitName m2) = MolecularUnitName $ M.unionWith (P.+) m1 m2

instance (Ord a) => Monoid (MolecularUnitName a) where
  mempty = MolecularUnitName M.empty
  mappend = (<>)

instance (Ord a) => Group (MolecularUnitName a) where
  invert (MolecularUnitName m) = MolecularUnitName $ fmap P.negate m
  pow (MolecularUnitName m) x = MolecularUnitName $ fmap (P.* (fromIntegral x)) m

instance (Ord a) => Abelian (MolecularUnitName a)

productNormalForm :: (Ord a) => UnitName' m a -> UnitName' m a
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
    go :: (Ord a) => UnitName' 'NonMetric a -> UnitName' 'NonMetric a
    go = productOfMolecules . toMolecules

quotientNormalForm :: (Ord a) => UnitName' m a -> UnitName' m a
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
    go :: (Ord a) => UnitName' 'NonMetric a -> UnitName' 'NonMetric a
    go = fromMolecules . partitionMolecules . toMolecules
    partitionMolecules :: MolecularUnitName a -> (MolecularUnitName a, MolecularUnitName a)
    partitionMolecules (MolecularUnitName m) = (\(x,y) -> (MolecularUnitName x, MolecularUnitName y)) . M.partition (> 0) . M.filter (/= 0) $ m
    fromMolecules :: (MolecularUnitName a, MolecularUnitName a) -> UnitName' 'NonMetric a
    fromMolecules (ns, ds) = case productOfMolecules ds of
                               One -> productOfMolecules ns
                               ds' -> Quotient (productOfMolecules ns) ds'

productOfMolecules :: MolecularUnitName a -> UnitName' 'NonMetric a
productOfMolecules (MolecularUnitName m) = product . fmap build . M.toList $ m
  where
    build :: (NameMolecule a, Int) -> UnitName' 'NonMetric a
    build (_, 0) = One
    build ((NameMolecule (Just p) a), 1) = Prefixed p a
    build ((NameMolecule Nothing a), 1) = Atomic a
    build ((NameMolecule (Just p) a), x) = Power (Prefixed p a) x
    build ((NameMolecule Nothing a), x) = Power (Atomic a) x

toMolecules :: (Ord a) => UnitName' m a -> MolecularUnitName a
toMolecules = evaluateMolecules (\m -> MolecularUnitName $ M.singleton m 1)

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
ucumName = foldName f . fmap (nameComponent ucum) . ensureSimpleDenominatorsAndPowers . distributePowers . unitName
  where
    f = UnitNameFold {
      foldOne = Just "1"
    , foldPrefix = liftA2 (++)
    , foldProduct = \n1 n2 -> do
                                n1' <- n1
                                n2' <- n2
                                return $ n1' ++ "." ++ n2'
    , foldQuotient = \n1 n2 -> do
                                 n1' <- n1
                                 n2' <- n2
                                 return $ n1' ++ "/" ++ n2'
    , foldPower = \n x -> do
                            n' <- n
                            return $ n' ++ show x
    , foldGrouped = \n -> do
                            n' <- n
                            return $ "(" ++ n' ++ ")"
    }

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
    go [n] = n
    go (n : ns) = Product n (go ns)
