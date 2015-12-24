{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- for nested application of Concat

module Numeric.Units.Dimensional.Vectors
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

class VectorSpace (v :: *) where
  type Dimensions v :: [Dimension]
  fromListWithLeftovers :: Real b => [AnyQuantity b] -> (Maybe v, [AnyQuantity b])
  toList :: Fractional a => v -> [AnyQuantity a]
  zeroV :: v
  (^+^) :: v -> v -> v
  negateV :: v -> v
  negateV x = zeroV ^-^ x
  (^-^) :: v -> v -> v
  x ^-^ y = x ^+^ negateV y
  asVector :: forall a.(Real a, Fractional a, VectorSpace (Vector (Dimensions v) a)) => v -> Vector (Dimensions v) a
  asVector = fromJust . fromList . (toList :: v -> [AnyQuantity a])
  {-# MINIMAL fromListWithLeftovers, toList, zeroV, (^+^), (negateV | (^-^)) #-}

infixl 6 ^+^, ^-^

fromList :: (VectorSpace v, Real b) => [AnyQuantity b] -> Maybe v
fromList xs | (result, []) <- fromListWithLeftovers xs = result
            | otherwise = Nothing

-- scale really shouldn't be here, because this might be an affine type
class (VectorSpace v) => MonoVectorSpace (v :: *) where
  type Element v :: *
  fromMonoListWithLeftovers :: [AnyQuantity (Element v)] -> (Maybe v, [AnyQuantity (Element v)])
  toMonoList :: v -> [AnyQuantity (Element v)]
  scale :: Scalar v -> v -> v

fromMonoList :: (MonoVectorSpace v) => [AnyQuantity (Element v)] -> Maybe v
fromMonoList xs | (result, []) <- fromMonoListWithLeftovers xs = result
                | otherwise = Nothing

type Scalar v = Dimensionless (Element v)

gscale :: (Num a) => Quantity d a -> Vector ds a -> Vector (MapMul d ds) a
gscale x (VCons y v) = VCons (x * y) (gscale x v)
gscale _ VNil = VNil

infixr 7 ^/
(^/) :: (MonoVectorSpace v, Fractional (Element v)) => v -> Scalar v -> v
x ^/ s = scale (recip s) x

infixl 7 ^*, *^
(^*) :: (MonoVectorSpace v) => v -> Scalar v -> v
(^*) = flip scale

(*^) :: (MonoVectorSpace v) => Scalar v -> v -> v
(*^) = scale

lerp :: (MonoVectorSpace v) => v -> v -> Scalar v -> v
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

class MetricSpace (v :: * -> *) where
  type DistanceDimension v :: Dimension
  distance :: (Floating a) =>  v a -> v a -> Quantity (DistanceDimension v) a
  distance x y = Quantity . P.sqrt . unQuantity $ quadrance x y -- This implementation cannot use the dimensionally typed sqrt because it can't deduce that the square root of the squared dimension is the same
  quadrance :: (Floating a) => v a -> v a -> Quantity ((DistanceDimension v) ^ 'Pos2) a
  quadrance x y = d ^ pos2
    where
      d = distance x y
  {-# MINIMAL distance | quadrance #-}

-- Individual quantities can be viewed as single element vectors
instance (Real a, Fractional a, KnownDimension d) => VectorSpace (Quantity d a) where
  type Dimensions (Quantity d a) = '[d]
  fromListWithLeftovers (x : xs) = (fmap changeRep . promoteQuantity $ x, xs)
  fromListWithLeftovers [] = (Nothing, [])
  toList x = [demoteQuantity . changeRep $ x]
  zeroV = _0
  (^+^) = (+)
  negateV = negate
  (^-^) = (-)

instance (Real a, Fractional a, KnownDimension d) => MonoVectorSpace (Quantity d a) where
  type Element (Quantity d a) = a
  fromMonoListWithLeftovers (x : xs) = (promoteQuantity x, xs)
  fromMonoListWithLeftovers [] = (Nothing, [])
  toMonoList x = [demoteQuantity x]
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

instance VectorSpace (Vector '[] a) where
  type Dimensions (Vector '[] a) = '[]
  fromListWithLeftovers xs = (Just VNil, xs)
  toList VNil = []
  zeroV = VNil
  _ ^+^ _ = VNil
  negateV = const VNil
  _ ^-^ _ = VNil

instance (Real a, Fractional a, KnownDimension d, VectorSpace (Vector ds a)) => VectorSpace (Vector (d ': ds) a) where
  type Dimensions (Vector (d ': ds) a) = d ': ds
  fromListWithLeftovers (x : xs) = (liftA2 VCons x' xs', left)
    where
      x' = fmap changeRep . promoteQuantity $ x
      (xs', left) = fromListWithLeftovers xs
  fromListWithLeftovers [] = (Nothing, [])
  toList (VCons x v) = (demoteQuantity . changeRep $ x) : toList v
  zeroV = VCons zeroV zeroV
  (VCons x1 v1) ^+^ (VCons x2 v2) = VCons (x1 ^+^ x2) (v1 ^+^ v2)
  negateV (VCons x v) = VCons (negateV x) (negateV v)
  (VCons x1 v1) ^-^ (VCons x2 v2) = VCons (x1 ^-^ x2) (v1 ^-^ v2)

instance MonoVectorSpace (Vector '[] a) where
  type Element (Vector '[] a) = a
  fromMonoListWithLeftovers xs = (Just VNil, xs)
  toMonoList VNil = []
  scale _ _ = VNil

instance (Real a, Fractional a, KnownDimension d, a ~ Element (Vector ds a), MonoVectorSpace (Vector ds a)) => MonoVectorSpace (Vector (d ': ds) a) where
  type Element (Vector (d ': ds) a) = a
  fromMonoListWithLeftovers (x : xs) = (liftA2 VCons x' xs', left)
    where
      x' = promoteQuantity x
      (xs', left) = fromMonoListWithLeftovers xs
  fromMonoListWithLeftovers [] = (Nothing, [])
  toMonoList (VCons x v) = (demoteQuantity x) : toMonoList v
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

-- The mention of d here is necessary to prevent this instance from overlapping with the base case of Vector '[DLength] a
instance (d ~ (DistanceDimension (Vector (d' ': ds))), MetricSpace (Vector (d' ': ds))) => MetricSpace (Vector (d ': d' ': ds)) where
  type DistanceDimension (Vector (d ': d' ': ds)) = d
  quadrance (VCons x1 v1) (VCons x2 v2) = quadrance x1 x2 + quadrance v1 v2

instance Show (Vector '[] a) where
  show VNil = "<| |>"

instance (Show a, Real a, Fractional a, KnownDimension d, a ~ Element (Vector ds a), MonoVectorSpace (Vector ds a)) => Show (Vector (d ': ds) a) where
  show v = "<| " ++ elems v ++ " |>"
    where
      elems = intercalate ", " . fmap show . toMonoList

-- | In a 'MetricSpace' that is also a 'VectorSpace' the 'lengthV' of a vector is its 'distance' from the zero vector, 'zeroV'.
lengthV :: (VectorSpace (v a), MetricSpace v, Floating a) => v a -> Quantity (DistanceDimension v) a
lengthV = distance zeroV

-- | In a 'MonoVectorSpace' that is also a 'MetricSpace' with a 'DistanceDimension' of 'DOne', it's possible to
-- 'scale' a vector by the reciprocal of it's 'length', obtaining a normalized version of the vector.
normalize :: (MonoVectorSpace (v a), MetricSpace v, DOne ~ DistanceDimension v, a ~ Element (v a), Floating a) => v a -> v a
normalize x = scale (recip . lengthV $ x) x

type DirectionVector v a = UnitV (Vector (MapMul (Recip (DistanceDimension v)) (Dimensions (v a)))) a

-- | Converts a vector in a metric space (therefore, one with homogenous dimensions) into a direction vector.
direction :: (VectorSpace (v a), MetricSpace v, Real a, Floating a, VectorSpace (Vector (Dimensions (v a)) a)) => v a -> DirectionVector v a
direction v = UnitV . gscale (recip . lengthV $ v) . asVector $ v

type V2 d = Vector '[d, d]
type V3 d = Vector '[d, d, d]
type V4 d = Vector '[d, d, d, d]

-- Augmented Vectors
data Augmented (v1 :: *) (v2 :: *) = Augmented !v1 !v2

data AugmentedMono (v1 :: * -> *) (v2 :: * -> *) a = AugmentedMono !(v1 a) !(v2 a)

instance (VectorSpace v1, VectorSpace v2) => VectorSpace (Augmented v1 v2) where
  type Dimensions (Augmented v1 v2) = Concat (Dimensions v1) (Dimensions v2)
  fromListWithLeftovers xs = (liftA2 Augmented v1 v2, xs'')
    where
      (v1, xs') = fromListWithLeftovers xs
      (v2, xs'') = fromListWithLeftovers xs'
  toList (Augmented v1 v2) = toList v1 ++ toList v2
  zeroV = Augmented zeroV zeroV
  (Augmented x1 y1) ^+^ (Augmented x2 y2) = Augmented (x1 ^+^ x2) (y1 ^+^ y2)
  negateV (Augmented v1 v2) = Augmented (negateV v1) (negateV v2)

instance (MonoVectorSpace v1, MonoVectorSpace v2, Element v1 ~ Element v2) => MonoVectorSpace (Augmented v1 v2) where
  type Element (Augmented v1 v2) = Element v1
  fromMonoListWithLeftovers xs = (liftA2 Augmented v1 v2, xs'')
    where
      (v1, xs') = fromMonoListWithLeftovers xs
      (v2, xs'') = fromMonoListWithLeftovers xs'
  toMonoList (Augmented v1 v2) = toMonoList v1 ++ toMonoList v2
  scale s (Augmented v1 v2) = Augmented (scale s v1) (scale s v2)

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

instance (VectorSpace (v1 a), VectorSpace (v2 a)) => VectorSpace (AugmentedMono v1 v2 a) where
  type Dimensions (AugmentedMono v1 v2 a) = Concat (Dimensions (v1 a)) (Dimensions (v2 a))
  fromListWithLeftovers xs = (liftA2 AugmentedMono v1 v2, xs'')
    where
      (v1, xs') = fromListWithLeftovers xs
      (v2, xs'') = fromListWithLeftovers xs'
  toList (AugmentedMono v1 v2) = toList v1 ++ toList v2
  zeroV = (AugmentedMono zeroV zeroV)
  (AugmentedMono x1 y1) ^+^ (AugmentedMono x2 y2) = AugmentedMono (x1 ^+^ x2) (y1 ^+^ y2)
  negateV (AugmentedMono v1 v2) = AugmentedMono (negateV v1) (negateV v2)

instance (MonoVectorSpace (v1 a), MonoVectorSpace (v2 a), Element (v1 a) ~ Element (v2 a)) => MonoVectorSpace (AugmentedMono v1 v2 a) where
  type Element (AugmentedMono v1 v2 a) = Element (v1 a)
  fromMonoListWithLeftovers xs = (liftA2 AugmentedMono v1 v2, xs'')
    where
      (v1, xs') = fromMonoListWithLeftovers xs
      (v2, xs'') = fromMonoListWithLeftovers xs'
  toMonoList (AugmentedMono v1 v2) = toMonoList v1 ++ toMonoList v2
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

newtype UnitV v a = UnitV { unUnitV :: (v a) }
  deriving (Eq)

instance Show (v a) => Show (UnitV v a) where
  show = show . unUnitV

unitV :: (MonoVectorSpace (v a), MetricSpace v, DOne ~ DistanceDimension v, a ~ Element (v a), Floating a) => v a -> UnitV v a
unitV = UnitV . normalize
