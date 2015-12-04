{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Numeric.Units.Dimensional.Vectors
where

import Data.Maybe
import Data.Proxy
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.Dynamic hiding ((*), recip)

class Vector (v :: *) where
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
class (Vector v) => MonoVector (v :: *) where
  type Element v :: *
  fromMonoList :: [AnyQuantity (Element v)] -> Maybe v
  toMonoList :: v -> [AnyQuantity (Element v)]
  fromRawList :: [Element v] -> Maybe v
  toRawList :: v -> [Element v]
  scale :: Scalar v -> v -> v

type Scalar v = Dimensionless (Element v)

infixr 7 ^/
(^/) :: (MonoVector v, Fractional (Element v)) => v -> Scalar v -> v
x ^/ s = scale (recip s) x

infixl 7 ^*, *^
(^*) :: (MonoVector v) => v -> Scalar v -> v
(^*) = flip scale

(*^) :: (MonoVector v) => Scalar v -> v -> v
(*^) = scale

lerp :: (MonoVector v) => v -> v -> Scalar v -> v
lerp a b t = a ^+^ t *^ (b ^-^ a)

changeVectorRep :: forall v1 v2 a.(Vector v1, Vector v2, Real a, Fractional a, Dimensions v1 ~ Dimensions v2) => Proxy a -> v1 -> v2
changeVectorRep _ = fromJust . fromList . (toList :: v1 -> [AnyQuantity a])

-- Individual quantities can be viewed as single element vectors
instance (Real a, Fractional a, KnownDimension d) => Vector (Quantity d a) where
  type Dimensions (Quantity d a) = '[d]
  fromList [x] = fmap changeRep . promoteQuantity $ x
  fromList _   = Nothing
  toList x = [demoteQuantity . changeRep $ x]
  zeroV = _0
  (^+^) = (+)
  negateV = negate
  (^-^) = (-)

instance (Real a, Fractional a, KnownDimension d) => MonoVector (Quantity d a) where
  type Element (Quantity d a) = a
  fromMonoList [x] = promoteQuantity x
  toMonoList x = [demoteQuantity x]
  fromRawList [x] = Just $ x *~ siUnit
  fromRawList _   = Nothing
  toRawList x = [x /~ siUnit]
  scale s x = s * x
