{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- for nested application of Concat

module Numeric.Units.Dimensional.Vectors
(
  -- * Vectors as Lists of Quantities
  -- ** Of Kind '*'
  CVectorMono(..), fromList
  -- ** Of Kind @ * -> * @
, CVector(..), fromMonoList
  -- * Vector Spaces
  -- ** Of Kind '*'
, VectorSpace(..)
  -- ** Of Kind @ * -> * @
, MonoVectorSpace(..), gscale, (^*), (*^), lerp, changeVectorRep
  -- * Affine Spaces
, AffineSpace(..), (.-^)
  -- * Metric Spaces
, MetricSpace(..), lengthV, normalize
  -- ** Unit Vectors
, UnitV(..), unitV
  -- ** Direction Vectors
, DirectionVector, DirectionSpace, direction
  -- * General-Purpose Vector Types
, Vector(..)
  -- ** Augmented Vectors
, Augmented(..), AugmentedMono(..)
  -- ** Commonly Used Type Synonyms
, V0, V1, V2, V3, V4
  -- ** Commonly Used Pattern Synonyms
, pattern V1, pattern V2, pattern V3, pattern V4
  -- * Utility Functions for Type Level Lists
, DimensionallyHomogenous(..)
, MapMul, Concat
)
where

import Control.Applicative
import Data.List (intercalate)
import Data.Maybe
import Data.Proxy
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.Coercion
import Numeric.Units.Dimensional.Dynamic hiding ((*), (^), recip)
import Numeric.NumType.DK.Integers
  ( TypeInt (Pos2) )
import qualified Prelude as P

type family MapMul (d :: Dimension) (ds :: [Dimension]) :: [Dimension] where
  MapMul d' '[] = '[]
  MapMul d' (d ': ds) = (d' * d) ': (MapMul d' ds)

type family Concat (xs :: [k]) (ys :: [k]) :: [k] where
  Concat '[]       ys = ys
  Concat (x ': xs) ys = x ': (Concat xs ys)

class DimensionallyHomogenous (ds :: [Dimension]) where
  type HomogenousDimension ds :: Dimension

instance DimensionallyHomogenous '[d] where
  type HomogenousDimension '[d] = d

instance (d ~ HomogenousDimension ds) => DimensionallyHomogenous (d ': ds) where
  type HomogenousDimension (d ': ds) = d

-- | The type of representationally-heterogenous vector-like types, composed of a list of values with fixed dimensions.
--
-- Note that the type need not necessarily be a 'VectorSpace'. Types representing points in affine spaces
-- may be instances of 'CVectorMono'. In that sense the use of "vector" here is more aligned with its usual
-- meaning in computer science than with its usual meaning in mathematics.
class CVectorMono (v :: *) where
  -- | A list of the 'Dimension' of each element in a vector-like type.
  type Dimensions v :: [Dimension]
  -- | Construct a representationally-heterogenous vector-like type from a list of 'Promotable' values, if sufficiently many
  -- values are available and their dimensions match the 'Dimensions' of the vector-like type. Return the
  -- result, if any, along with a list of any remaining values in the input list.
  --
  -- Clients are likely to prefer 'fromList', but this type is needed for implementation.
  fromListWithLeftovers :: (Promotable p, Real b) => [p b] -> (Maybe v, [p b])
  -- | Convert a representationally-heterogenous vector-like type to a list of 'Promotable' values.
  --
  -- The length and dimensions of the result list must match the 'Dimensions' of the vector-like type.
  toList :: (Promotable p, Fractional a) => v -> [p a]
  -- | Convert a representationally-heterogenous vector-like type to a 'Vector' of the same 'Dimensions'.
  toVector :: forall a.(Real a, Fractional a, VectorSpace (Vector (Dimensions v) a)) => v -> Vector (Dimensions v) a
  toVector = fromJust . fromList . (toList :: v -> [AnyQuantity a])
    -- The types here promise us that the fromJust will succeed.
  -- | Convert a 'Vector' to a representationally-heterogenous vector-like type of the same 'Dimensions'.
  fromVector :: forall a.(Real a, Fractional a, VectorSpace (Vector (Dimensions v) a)) => Vector (Dimensions v) a -> v
  fromVector = fromJust . fromList . (toList :: Vector (Dimensions v) a -> [AnyQuantity a])
    -- The types here promise us that the fromJust will succeed.
  {-# MINIMAL fromListWithLeftovers, toList #-}

class (CVectorMono v) => VectorSpace (v :: *) where
  zeroV :: v
  (^+^) :: v -> v -> v
  negateV :: v -> v
  negateV x = zeroV ^-^ x
  (^-^) :: v -> v -> v
  x ^-^ y = x ^+^ negateV y
  {-# MINIMAL zeroV, (^+^), (negateV | (^-^)) #-}

infixl 6 ^+^, ^-^

-- | Construct a representationally-heterogenous vector-like type from a list of 'Promotable' values, if precisely as many
-- values are available as are required and their dimensions match the 'Dimensions' of the vector-like type. Return 'Nothing'
-- if these conditions are not met.
fromList :: (Promotable p, CVectorMono v, Real a) => [p a] -> Maybe v
fromList xs | (result, []) <- fromListWithLeftovers xs = result
            | otherwise = Nothing

-- | The type of representationally-homogenous vector-like types, composed of a list of values with fixed dimensions.
--
-- Note that the type need not necessarily be a 'VectorSpace'. Types representing points in affine spaces
-- may be instances of 'CVector'. In that sense the use of "vector" here is more aligned with its usual
-- meaning in computer science than with its usual meaning in mathematics.
class CVector (v :: * -> *) where
  -- | Construct a representationally-homogenous vector-like type from a list of 'Promotable' values, if sufficiently many
  -- values are available and their dimensions match the 'Dimensions' of the vector-like type. Return the
  -- result, if any, along with a list of any remaining values in the input list.
  --
  -- Clients are likely to prefer 'fromMonoList', but this type is needed for implementation.
  fromMonoListWithLeftovers :: (Promotable p) => [p a] -> (Maybe (v a), [p a])
  -- | Convert a representationally-homogenous vector-like type to a list of 'Promotable' values.
  --
  -- The length and dimensions of the result list must match the 'Dimensions' of the vector-like type.
  toMonoList :: (Promotable p) => v a -> [p a]

-- scale really shouldn't be here, because this might be an affine type
class (CVector v) => MonoVectorSpace (v :: * -> *) where
  scale :: (Num a) => Dimensionless a -> v a -> v a

-- | Construct a representationally-homogenous vector-like type from a list of 'Promotable' values, if precisely as many
-- values are available as are required and their dimensions match the 'Dimensions' of the vector-like type. Return 'Nothing'
-- if these conditions are not met.
fromMonoList :: (Promotable p, CVector v) => [p a] -> Maybe (v a)
fromMonoList xs | (result, []) <- fromMonoListWithLeftovers xs = result
                | otherwise = Nothing

-- | Scales a representationally-homogenous 'Vector' by a 'Quantity' of arbitrary dimension,
-- forming a new 'Vector' whose dimensions are the element-wise product of the dimensions of the input
-- quantity and the input vector.
gscale :: (Num a) => Quantity d a -> Vector ds a -> Vector (MapMul d ds) a
gscale x (VCons y v) = VCons (x * y) (gscale x v)
gscale _ VNil = VNil

infixl 7 ^*, *^
(^*) :: (MonoVectorSpace v, Num a) => v a -> Dimensionless a -> v a
(^*) = flip scale

(*^) :: (MonoVectorSpace v, Num a) => Dimensionless a -> v a -> v a
(*^) = scale

-- | Linearly interpolates (or extrapolates) between two vectors.
lerp :: (MonoVectorSpace v, Num a, VectorSpace (v a)) => v a -> v a -> Dimensionless a -> v a
lerp a b t = a ^+^ t *^ (b ^-^ a)

changeVectorRep :: forall v1 v2 a.(VectorSpace v1, VectorSpace v2, Real a, Fractional a, Dimensions v1 ~ Dimensions v2) => Proxy a -> v1 -> v2
changeVectorRep _ = fromJust . fromList . (toList :: v1 -> [AnyQuantity a])

class VectorSpace (Diff p) => AffineSpace (p :: *) where
  type Diff p :: *
  (.-.) :: p -> p -> Diff p
  (.+^) :: p -> Diff p -> p

infix 6 .-.
infixl 6 .+^

(.-^) :: AffineSpace p => p -> Diff p -> p
p .-^ v = p .+^ negateV v

infixl 6 .-^

-- | A space in which we can compute the 'distance' between any two values, which will be a 'Quantity'
-- of a specified 'Dimension', the 'DistanceDimension' of the 'MetricSpace'.
class MetricSpace (v :: * -> *) where
  -- | Obtains the 'Dimension' used to measure distances in a 'MetricSpace'.
  type DistanceDimension v :: Dimension
  -- | Calculates the distance between two values in a metric space.
  distance :: (Floating a) =>  v a -> v a -> Quantity (DistanceDimension v) a
  distance x y = Quantity . P.sqrt . unQuantity $ quadrance x y -- This implementation cannot use the dimensionally typed sqrt because it can't deduce that the square root of the squared dimension is the same
  -- | Calculates the square of the distance between two values in a metric space.
  --
  -- This is provided because it makes writing recursive instances much easier.
  quadrance :: (Floating a) => v a -> v a -> Quantity ((DistanceDimension v) ^ 'Pos2) a
  quadrance x y = d ^ pos2
    where
      d = distance x y
  {-# MINIMAL distance | quadrance #-}

-- Individual quantities can be viewed as single element vectors
instance (Real a, Fractional a, KnownDimension d) => CVectorMono (Quantity d a) where
  type Dimensions (Quantity d a) = '[d]
  fromListWithLeftovers (x : xs) = (fmap changeRep . promoteQuantity $ x, xs)
  fromListWithLeftovers [] = (Nothing, [])
  toList x = [demoteQuantity . changeRep $ x]

instance (Real a, Fractional a, KnownDimension d) => VectorSpace (Quantity d a) where
  zeroV = _0
  (^+^) = (+)
  negateV = negate
  (^-^) = (-)

instance (KnownDimension d) => CVector (Quantity d) where
  fromMonoListWithLeftovers (x : xs) = (promoteQuantity x, xs)
  fromMonoListWithLeftovers [] = (Nothing, [])
  toMonoList x = [demoteQuantity x]

instance (KnownDimension d) => MonoVectorSpace (Quantity d) where
  scale s x = s * x

instance (Real a, Fractional a, KnownDimension d) => AffineSpace (Quantity d a) where
  type Diff (Quantity d a) = Quantity d a
  (.-.) = (-)
  (.+^) = (+)

instance MetricSpace (Quantity d) where
  type DistanceDimension (Quantity d) = d
  distance x y = abs $ x - y
  quadrance x y = (x - y) ^ pos2

-- General purpose vectors.
data Vector (ds :: [Dimension]) a where
  VCons :: !(Quantity d a) -> !(Vector ds a) -> Vector (d ': ds) a
  VNil  :: Vector '[] a

deriving instance (Eq a) => Eq (Vector ds a)

instance Storable (Vector '[] a) where
  sizeOf _ = 0
  {-# INLINE sizeOf #-}
  alignment _ = 1
  {-# INLINE alignment #-}
  poke _ _ = return ()
  {-# INLINE poke #-}
  peek _ = return VNil
  {-# INLINE peek #-}

instance (Storable a, Storable (Vector ds a)) => Storable (Vector (d ': ds) a) where
  sizeOf _ = sizeOf (undefined::a) P.+ sizeOf (undefined :: Vector ds a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::a)
  {-# INLINE alignment #-}
  poke ptr (VCons x xs) = do
                            poke (castPtr ptr) x
                            pokeByteOff (castPtr ptr) (sizeOf (undefined :: a)) xs
  {-# INLINE poke #-}
  peek ptr = do
               x <- peek (castPtr ptr)
               xs <- peekByteOff (castPtr ptr) (sizeOf (undefined :: a))
               return $ VCons x xs
  {-# INLINE peek #-}

instance CVectorMono (Vector '[] a) where
  type Dimensions (Vector '[] a) = '[]
  fromListWithLeftovers xs = (Just VNil, xs)
  toList VNil = []

instance (Real a, Fractional a, KnownDimension d, CVectorMono (Vector ds a)) => CVectorMono (Vector (d ': ds) a) where
  type Dimensions (Vector (d ': ds) a) = d ': ds
  fromListWithLeftovers (x : xs) = (liftA2 VCons x' xs', left)
    where
      x' = fmap changeRep . promoteQuantity $ x
      (xs', left) = fromListWithLeftovers xs
  fromListWithLeftovers [] = (Nothing, [])
  toList (VCons x v) = (demoteQuantity . changeRep $ x) : toList v

instance VectorSpace (Vector '[] a) where
  zeroV = VNil
  _ ^+^ _ = VNil
  negateV = const VNil
  _ ^-^ _ = VNil

instance (Real a, Fractional a, KnownDimension d, VectorSpace (Vector ds a)) => VectorSpace (Vector (d ': ds) a) where
  zeroV = VCons zeroV zeroV
  (VCons x1 v1) ^+^ (VCons x2 v2) = VCons (x1 ^+^ x2) (v1 ^+^ v2)
  negateV (VCons x v) = VCons (negateV x) (negateV v)
  (VCons x1 v1) ^-^ (VCons x2 v2) = VCons (x1 ^-^ x2) (v1 ^-^ v2)

instance CVector (Vector '[]) where
  fromMonoListWithLeftovers xs = (Just VNil, xs)
  toMonoList VNil = []

instance (CVector (Vector ds), KnownDimension d) => CVector (Vector (d ': ds)) where
  fromMonoListWithLeftovers (x : xs) = (liftA2 VCons x' xs', left)
    where
      x' = promoteQuantity x
      (xs', left) = fromMonoListWithLeftovers xs
  fromMonoListWithLeftovers [] = (Nothing, [])
  toMonoList (VCons x v) = (demoteQuantity x) : toMonoList v

instance MonoVectorSpace (Vector '[]) where
  scale _ _ = VNil

instance (KnownDimension d, MonoVectorSpace (Vector ds)) => MonoVectorSpace (Vector (d ': ds)) where  
  scale s (VCons x v) = VCons (scale s x) (scale s v)

instance MetricSpace (Vector '[d]) where
  type DistanceDimension (Vector '[d]) = d
  quadrance (VCons x1 VNil) (VCons x2 VNil) = quadrance x1 x2
  quadrance _ _ = error "Unreachable."

instance AffineSpace (Vector '[] a) where
  type Diff (Vector '[] a) = Vector '[] a
  (.-.) = (^-^)
  (.+^) = (^+^)

instance (Real a, Fractional a, KnownDimension d, VectorSpace (Vector ds a)) => AffineSpace (Vector (d ': ds) a) where
  type Diff (Vector (d ': ds) a) = Vector (d ': ds) a
  (.-.) = (^-^)
  (.+^) = (^+^)

-- The mention of d here is necessary to prevent this instance from overlapping with the base case of Vector '[d] a
instance (d ~ (DistanceDimension (Vector (d' ': ds))), MetricSpace (Vector (d' ': ds))) => MetricSpace (Vector (d ': d' ': ds)) where
  type DistanceDimension (Vector (d ': d' ': ds)) = d
  quadrance (VCons x1 v1) (VCons x2 v2) = quadrance x1 x2 + quadrance v1 v2

instance Show (Vector '[] a) where
  show VNil = "<| |>"

instance (Show a, Real a, Fractional a, KnownDimension d, MonoVectorSpace (Vector ds)) => Show (Vector (d ': ds) a) where
  show v = "<| " ++ elems v ++ " |>"
    where
      elems = intercalate ", " . fmap show . (\x -> x :: [AnyQuantity a]) . toMonoList

-- | In a 'MetricSpace' that is also a 'VectorSpace' the 'lengthV' of a vector is its 'distance' from the zero vector, 'zeroV'.
lengthV :: (VectorSpace (v a), MetricSpace v, Floating a) => v a -> Quantity (DistanceDimension v) a
lengthV = distance zeroV

-- | In a 'MonoVectorSpace' that is also a 'MetricSpace' with a 'DistanceDimension' of 'DOne', it's possible to
-- 'scale' a vector by the reciprocal of it's 'length', obtaining a normalized version of the vector.
normalize :: (MonoVectorSpace v, MetricSpace v, DOne ~ DistanceDimension v, VectorSpace (v a), Floating a) => v a -> v a
normalize x = scale (recip . lengthV $ x) x

-- | The type of a direction vector corresponding with a dimensionally homogenous vector type.
type DirectionVector v a = UnitV
                           (
                             Vector 
                             (
                               MapMul 
                                 (Recip (HomogenousDimension (Dimensions (v a))))
                                 (Dimensions (v a))
                             )
                           )
                           a

-- | Classifies the type of vectors for which we can take a 'direction'.
type DirectionSpace v a = (VectorSpace (v a), MetricSpace v, DistanceDimension v ~ HomogenousDimension (Dimensions (v a)), Real a, Floating a, VectorSpace (Vector (Dimensions (v a)) a))

-- | Converts a vector in a metric space with homogenous dimensions into a direction vector.
--
-- Each component of the direction vector will be 'Dimensionless'.
direction :: DirectionSpace v a => v a -> DirectionVector v a
direction v = UnitV . gscale (recip . lengthV $ v) . toVector $ v

-- | A 'Vector' with zero elements.
type V0 = Vector '[]

-- | A 'Vector' with one element.
type V1 d = Vector '[d]

-- | A dimensionally homogenous 'Vector' with two elements.
type V2 d = Vector '[d, d]

-- | A dimensionally homogenous 'Vector' with three elements.
type V3 d = Vector '[d, d, d]

-- | A dimensionally homogenous 'Vector' with four elements.
type V4 d = Vector '[d, d, d, d]

-- | Matches or constructs a 'Vector' with one element.
pattern V1 :: () => () => Quantity d1 a -> Vector '[d1] a
pattern V1 x = VCons x VNil

-- | Matches or constructs a 'Vector' with two elements, which need not be dimensionally homogenous.
pattern V2 :: () => () => Quantity d1 a -> Quantity d2 a -> Vector '[d1, d2] a
pattern V2 x y = VCons x (VCons y VNil)

-- | Matches or constructs a 'Vector' with three elements, which need not be dimensionally homogenous.
pattern V3 :: () => () => Quantity d1 a -> Quantity d2 a -> Quantity d3 a -> Vector '[d1, d2, d3] a
pattern V3 x y z = VCons x (VCons y (VCons z VNil))

-- | Matches or constructs a 'Vector' with four elements, which need not be dimensionally homogenous.
pattern V4 :: () => () => Quantity d1 a -> Quantity d2 a -> Quantity d3 a -> Quantity d4 a -> Vector '[d1, d2, d3, d4] a
pattern V4 w x y z = VCons w (VCons x (VCons y (VCons z VNil)))

-- Augmented Vectors
data Augmented (v1 :: *) (v2 :: *) = Augmented !v1 !v2

data AugmentedMono (v1 :: * -> *) (v2 :: * -> *) a = AugmentedMono !(v1 a) !(v2 a)

instance (CVectorMono v1, CVectorMono v2) => CVectorMono (Augmented v1 v2) where
  type Dimensions (Augmented v1 v2) = Concat (Dimensions v1) (Dimensions v2)
  fromListWithLeftovers xs = (liftA2 Augmented v1 v2, xs'')
    where
      (v1, xs') = fromListWithLeftovers xs
      (v2, xs'') = fromListWithLeftovers xs'
  toList (Augmented v1 v2) = toList v1 ++ toList v2

instance (VectorSpace v1, VectorSpace v2) => VectorSpace (Augmented v1 v2) where
  zeroV = Augmented zeroV zeroV
  (Augmented x1 y1) ^+^ (Augmented x2 y2) = Augmented (x1 ^+^ x2) (y1 ^+^ y2)
  negateV (Augmented v1 v2) = Augmented (negateV v1) (negateV v2)

instance (AffineSpace v1, AffineSpace v2) => AffineSpace (Augmented v1 v2) where
  type Diff (Augmented v1 v2) = Augmented (Diff v1) (Diff v2)
  (Augmented x1 y1) .-. (Augmented x2 y2) = Augmented (x1 .-. x2) (y1 .-. y2)
  (Augmented x y) .+^ (Augmented dx dy) = Augmented (x .+^ dx) (y .+^ dy)

instance (Storable v1, Storable v2) => Storable (Augmented v1 v2) where
  sizeOf _ = sizeOf (undefined :: v1) P.+ sizeOf (undefined :: v2)
  {-# INLINE sizeOf #-}
  alignment _ = max (alignment (undefined :: v1)) (alignment (undefined :: v2))
  {-# INLINE alignment #-}
  poke p (Augmented v1 v2) = do
                               poke (castPtr p) v1
                               pokeByteOff p (sizeOf v1) v2
  {-# INLINE poke #-}
  peek p = do
             v1 <- peek (castPtr p)
             v2 <- peekByteOff p (sizeOf v1)
             return $ Augmented v1 v2
  {-# INLINE peek #-}

instance (CVectorMono (v1 a), CVectorMono (v2 a)) => CVectorMono (AugmentedMono v1 v2 a) where
  type Dimensions (AugmentedMono v1 v2 a) = Concat (Dimensions (v1 a)) (Dimensions (v2 a))
  fromListWithLeftovers xs = (liftA2 AugmentedMono v1 v2, xs'')
    where
      (v1, xs') = fromListWithLeftovers xs
      (v2, xs'') = fromListWithLeftovers xs'
  toList (AugmentedMono v1 v2) = toList v1 ++ toList v2

instance (VectorSpace (v1 a), VectorSpace (v2 a)) => VectorSpace (AugmentedMono v1 v2 a) where
  zeroV = (AugmentedMono zeroV zeroV)
  (AugmentedMono x1 y1) ^+^ (AugmentedMono x2 y2) = AugmentedMono (x1 ^+^ x2) (y1 ^+^ y2)
  negateV (AugmentedMono v1 v2) = AugmentedMono (negateV v1) (negateV v2)

instance (CVector v1, CVector v2) => CVector (AugmentedMono v1 v2) where
  fromMonoListWithLeftovers xs = (liftA2 AugmentedMono v1 v2, xs'')
    where
      (v1, xs') = fromMonoListWithLeftovers xs
      (v2, xs'') = fromMonoListWithLeftovers xs'
  toMonoList (AugmentedMono v1 v2) = toMonoList v1 ++ toMonoList v2

instance (MonoVectorSpace v1, MonoVectorSpace v2) => MonoVectorSpace (AugmentedMono v1 v2) where
  scale s (AugmentedMono v1 v2) = AugmentedMono (scale s v1) (scale s v2)

instance (AffineSpace (v1 a), AffineSpace (v2 a)) => AffineSpace (AugmentedMono v1 v2 a) where
  type Diff (AugmentedMono v1 v2 a) = Augmented (Diff (v1 a)) (Diff (v2 a))
  (AugmentedMono x1 y1) .-. (AugmentedMono x2 y2) = Augmented (x1 .-. x2) (y1 .-. y2)
  (AugmentedMono x y) .+^ (Augmented dx dy) = AugmentedMono (x .+^ dx) (y .+^ dy)

instance (MetricSpace v1, MetricSpace v2, DistanceDimension v1 ~ DistanceDimension v2) => MetricSpace (AugmentedMono v1 v2) where
  type DistanceDimension (AugmentedMono v1 v2) = DistanceDimension v1
  quadrance (AugmentedMono x1 y1) (AugmentedMono x2 y2) = quadrance x1 x2 + quadrance y1 y2

instance (Storable (v1 a), Storable (v2 a)) => Storable (AugmentedMono v1 v2 a) where
  sizeOf _ = sizeOf (undefined :: v1 a) P.+ sizeOf (undefined :: v2 a)
  {-# INLINE sizeOf #-}
  alignment _ = max (alignment (undefined :: v1 a)) (alignment (undefined :: v2 a))
  {-# INLINE alignment #-}
  poke p (AugmentedMono v1 v2) = do
                               poke (castPtr p) v1
                               pokeByteOff p (sizeOf v1) v2
  {-# INLINE poke #-}
  peek p = do
             v1 <- peek (castPtr p)
             v2 <- peekByteOff p (sizeOf v1)
             return $ AugmentedMono v1 v2
  {-# INLINE peek #-}

instance (Show v1, Show v2) => Show (Augmented v1 v2) where
  show (Augmented v1 v2) = "(" ++ show v1 ++ ", " ++ show v2 ++ ")"

instance (Show (v1 a), Show (v2 a)) => Show (AugmentedMono v1 v2 a) where
  show (AugmentedMono v1 v2) = "(" ++ show v1 ++ ", " ++ show v2 ++ ")"

-- Torsors, by pretending to forget the origin and thus the ability to scale.
newtype Torsor v a = Torsor (v a)
  deriving (Eq, Ord)

instance (VectorSpace (v a)) => AffineSpace (Torsor v a) where
  type Diff (Torsor v a) = v a
  (Torsor p1) .-. (Torsor p2) = p1 ^-^ p2
  (Torsor p) .+^ x = Torsor $ p ^+^ x

instance (MetricSpace v) => MetricSpace (Torsor v) where
  type DistanceDimension (Torsor v) = DistanceDimension v
  distance (Torsor x) (Torsor y) = distance x y
  quadrance (Torsor x) (Torsor y) = quadrance x y

-- Unit Vectors

-- | A unit vector.
newtype UnitV v a = UnitV 
                    {
                      unUnitV :: (v a) -- ^ Unwraps a unit vector.
                    } -- ^ Unsafely constructs a unit vector by promising that the supplied value is normalized.
  deriving (Eq)

fmapOverFirst :: (Functor f) => (a -> b) -> (f a, x) -> (f b, x)
fmapOverFirst f (x, y) = (fmap f x, y)

instance (CVectorMono (v a), MonoVectorSpace v, MetricSpace v, DOne ~ DistanceDimension v, VectorSpace (v a), Floating a) => CVectorMono (UnitV v a) where
  type Dimensions (UnitV v a) = Dimensions (v a)
  fromListWithLeftovers = fmapOverFirst unitV . fromListWithLeftovers
  toList = toList . unUnitV

instance (MetricSpace v) => MetricSpace (UnitV v) where
  type DistanceDimension (UnitV v) = DistanceDimension v
  distance (UnitV x) (UnitV y) = distance x y
  quadrance (UnitV x) (UnitV y) = quadrance x y

instance Show (v a) => Show (UnitV v a) where
  show = show . unUnitV

-- | Safely constructs a unit vector from a dimensionless vector by normalizing it.
unitV :: (MonoVectorSpace v, MetricSpace v, DOne ~ DistanceDimension v, VectorSpace (v a), Floating a) => v a -> UnitV v a
unitV = UnitV . normalize
