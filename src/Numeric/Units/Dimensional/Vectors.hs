{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Numeric.Units.Dimensional.Vectors
where

import Data.Maybe
import Data.Proxy
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.Dynamic hiding ((*), recip)

class VectorSpace (v :: *) where
  type Dimensions v :: [Dimension]
  fromList :: Real b => [AnyQuantity b] -> Maybe v
  toList :: Fractional a => v -> [AnyQuantity a]
  zeroV :: v
  (^+^) :: v -> v -> v
  negateV :: v -> v
  negateV x = zeroV ^-^ x
  (^-^) :: v -> v -> v
  x ^-^ y = x ^+^ negateV y
  {-# MINIMAL fromList, toList, zeroV, (^+^), (negateV | (^-^)) #-}

infixl 6 ^+^, ^-^

-- scale really shouldn't be here, because this might be an affine type
class (VectorSpace v) => MonoVectorSpace (v :: *) where
  type Element v :: *
  fromMonoList :: [AnyQuantity (Element v)] -> Maybe v
  toMonoList :: v -> [AnyQuantity (Element v)]
  scale :: Scalar v -> v -> v

type Scalar v = Dimensionless (Element v)

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

class MetricSpace (v :: *) where
  type DistanceDimension v :: Dimension
  distance :: (Floating a) =>  v -> v -> Quantity (DistanceDimension v) a

-- Individual quantities can be viewed as single element vectors
instance (Real a, Fractional a, KnownDimension d) => VectorSpace (Quantity d a) where
  type Dimensions (Quantity d a) = '[d]
  fromList [x] = fmap changeRep . promoteQuantity $ x
  fromList _   = Nothing
  toList x = [demoteQuantity . changeRep $ x]
  zeroV = _0
  (^+^) = (+)
  negateV = negate
  (^-^) = (-)

instance (Real a, Fractional a, KnownDimension d) => MonoVectorSpace (Quantity d a) where
  type Element (Quantity d a) = a
  fromMonoList [x] = promoteQuantity x
  fromMonoList _ = Nothing
  toMonoList x = [demoteQuantity x]
  scale s x = s * x

instance (Real a, Fractional a, KnownDimension d) => MetricSpace (Quantity d a) where
  type DistanceDimension (Quantity d a) = d
  distance x y = changeRep $ x ^-^ y

data Vector (ds :: [Dimension]) a where
  VCons :: Quantity d a -> Vector ds a -> Vector (d ': ds) a
  VNil  :: Vector '[] a

deriving instance (Eq a) => Eq (Vector ds a)

instance VectorSpace (Vector '[] a) where
  type Dimensions (Vector '[] a) = '[]
  fromList [] = Just VNil
  fromList _  = Nothing
  toList VNil = []
  zeroV = VNil
  _ ^+^ _ = VNil
  negateV = const VNil
  _ ^-^ _ = VNil

instance (Real a, Fractional a, KnownDimension d, VectorSpace (Vector ds a)) => VectorSpace (Vector (d ': ds) a) where
  type Dimensions (Vector (d ': ds) a) = d ': ds
  fromList (x : xs) = do
                        x' <- fmap changeRep . promoteQuantity $ x
                        xs' <- fromList xs
                        return $ VCons x' xs'
  fromList _ = Nothing
  toList (VCons x v) = (demoteQuantity . changeRep $ x) : toList v
  zeroV = VCons zeroV zeroV
  (VCons x1 v1) ^+^ (VCons x2 v2) = VCons (x1 ^+^ x2) (v1 ^+^ v2)
  negateV (VCons x v) = VCons (negateV x) (negateV v)
  (VCons x1 v1) ^-^ (VCons x2 v2) = VCons (x1 ^-^ x2) (v1 ^-^ v2)

instance MonoVectorSpace (Vector '[] a) where
  type Element (Vector '[] a) = a
  fromMonoList [] = Just VNil
  fromMonoList _  = Nothing
  toMonoList VNil = []
  scale _ _ = VNil

instance (Real a, Fractional a, KnownDimension d, a ~ Element (Vector ds a), MonoVectorSpace (Vector ds a)) => MonoVectorSpace (Vector (d ': ds) a) where
  type Element (Vector (d ': ds) a) = a
  fromMonoList (x : xs) = do
                            x' <- promoteQuantity x
                            xs' <- fromMonoList xs
                            return $ VCons x' xs'
  fromMonoList _ = Nothing
  toMonoList (VCons x v) = (demoteQuantity x) : toMonoList v
  scale s (VCons x v) = VCons (scale s x) (scale s v)

-- Torsors, by pretending to forget the origin and thus the ability to scale.
newtype Torsor v = Torsor v
  deriving (Eq, Ord)

instance (VectorSpace v) => AffineSpace (Torsor v) where
  type Diff (Torsor v) = v
  (Torsor p1) .-. (Torsor p2) = p1 ^-^ p2
  (Torsor p) .+^ x = Torsor $ p ^+^ x

instance (MetricSpace v) => MetricSpace (Torsor v) where
  type DistanceDimension (Torsor v) = DistanceDimension v
  distance (Torsor x) (Torsor y) = distance x y
