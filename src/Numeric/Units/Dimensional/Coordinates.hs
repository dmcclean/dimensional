{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.Units.Dimensional.Coordinates where

import Control.Category
import Data.Coerce
import Data.Proxy
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Numeric.Units.Dimensional.Prelude hiding ((.), id, length)
import Numeric.Units.Dimensional.Vectors

data CoordinateType = Linear | Circular | Planar | Polar | Spherical | Cylindrical | Spatial -- what to name the 3D cartesian one is unclear

data CoordinateSystem = CoordinateSystem Symbol CoordinateType

type family Representation (t :: CoordinateType) :: [Dimension] where
  Representation 'Linear = '[DLength]
  Representation 'Circular = '[DPlaneAngle]
  Representation 'Planar = '[DLength, DLength]
  Representation 'Polar = '[DLength, DPlaneAngle]
  Representation 'Spherical = '[DLength, DPlaneAngle, DPlaneAngle]
  Representation 'Cylindrical = '[DLength, DPlaneAngle, DLength]
  Representation 'Spatial = '[DLength, DLength, DLength]

class KnownCoordinateType (ty :: CoordinateType) where
  canonicalize :: (Ord a, Floating a) => Proxy ty -> Vector (Representation ty) a -> Vector (Representation ty) a

instance KnownCoordinateType 'Linear where
  canonicalize _ = id

instance KnownCoordinateType 'Circular where
  canonicalize _ (VCons theta VNil) = VCons (singleTurnAngle theta) VNil
  canonicalize _ x = x -- GHC 7.10 can't realize that this case is unreachable

instance KnownCoordinateType 'Planar where
  canonicalize _ = id

instance KnownCoordinateType 'Polar where
  canonicalize _ (VCons r (VCons theta VNil)) | r < _0 = VCons (negate r) (VCons (singleTurnAngle (theta + pi)) VNil)
                                              | otherwise = VCons r (VCons (singleTurnAngle theta) VNil)
  canonicalize _ x = x -- GHC 7.10 can't realize that this case is unreachable

instance KnownCoordinateType 'Spherical where
  canonicalize _ (VCons r (VCons theta (VCons phi VNil))) = (VCons r (VCons theta (VCons phi VNil))) -- should constrain angles, non-negative radius
  canonicalize _ x = x -- GHC 7.10 can't realize that this case is unreachable

instance KnownCoordinateType 'Cylindrical where
  canonicalize _ (VCons r (VCons theta vz)) | r < _0 = VCons (negate r) (VCons (singleTurnAngle (theta + pi)) vz)
                                            | otherwise = VCons r (VCons (singleTurnAngle theta) vz)
  canonicalize _ x = x -- GHC 7.10 can't realize that this case is unreachable

instance KnownCoordinateType 'Spatial where
  canonicalize _ = id

singleTurnAngle :: (Floating a) => PlaneAngle a -> PlaneAngle a
singleTurnAngle = id

type family CoordinateSystemType (sys :: CoordinateSystem) :: CoordinateType where
  CoordinateSystemType ('CoordinateSystem sys ty) = ty

newtype Point (sys :: CoordinateSystem) a = Point (Vector (Representation (CoordinateSystemType sys)) a)

point :: forall sys a.(KnownCoordinateType (CoordinateSystemType sys), Ord a, Floating a) => Vector (Representation (CoordinateSystemType sys)) a -> Point sys a
point = Point . (canonicalize (Proxy :: Proxy (CoordinateSystemType sys)))

newtype Offset (sys :: CoordinateSystem) a = Offset (Vector (Representation (CoordinateSystemType sys)) a)

type ECEF = 'CoordinateSystem "ECEF" 'Spatial

here, there, doug, centerOfEarth :: Point ECEF Double
here = point $ VCons (3 *~ meter) (VCons (-12 *~ meter) (VCons (7 *~ meter) VNil))
there = point $ VCons (19.4 *~ meter) (VCons (171.9 *~ meter) (VCons (-41.6 *~ meter) VNil))
doug = point $ VCons (1557.123 *~ kilo meter) (VCons (-4471.044 *~ kilo meter) (VCons (4259.591 *~ kilo meter) VNil))
centerOfEarth = point zeroV

{-

Projection between coordinate systems

-}

data Projection (t :: *) (a :: CoordinateSystem) (b :: CoordinateSystem) where
  Identity :: (CoordinateSystemType a ~ CoordinateSystemType b) => Projection t a b
  Opaque :: (Point a t -> Point b t) -> Projection t a b
  Composed :: Projection t b c -> Projection t a b -> Projection t a c
  Translate :: (AffineSpace (Point sys t)) => Diff (Point sys t) -> Projection t sys sys
  RotateCircular :: (Real t, Fractional t) => PlaneAngle t -> Projection t ('CoordinateSystem a 'Circular) ('CoordinateSystem b 'Circular)
  RotatePlanar :: (Real t, Floating t) => PlaneAngle t -> Projection t ('CoordinateSystem a 'Planar) ('CoordinateSystem b 'Planar)
  RotatePolar :: (Real t, Fractional t) => PlaneAngle t -> Projection t ('CoordinateSystem a 'Polar) ('CoordinateSystem b 'Polar)
  -- | Converts planar coordinates to polar coordinates. The resulting polar coordinate system is centered at
  -- the origin and aligned with the positive x-axis.
  PlanarToPolar :: (RealFloat t) => Projection t ('CoordinateSystem a 'Planar) ('CoordinateSystem b 'Polar)
  -- | Converts polar coordinates to planar coordinates. The polar coordinate system is centered at
  -- the origin and aligned with the positive x-axis.
  PolarToPlanar :: (Floating t) => Projection t ('CoordinateSystem a 'Polar) ('CoordinateSystem b 'Planar)

instance Category (Projection t) where
  id = Identity
  (.) = Composed -- there are many opportunities for optimization here, eg merging consecutive projection matrices

project :: Projection t a b -> Point a t -> Point b t
project Identity = coerce
project (Opaque f) = f
project (Composed bc ab) = project bc . project ab
project (Translate offset) = (.+^ offset)
project (RotateCircular theta) = coerce f
  where
    f = (^+^ (VCons theta VNil))
project (RotatePlanar theta) = coerce f
  where
    f (VCons x (VCons y VNil)) = VCons x' (VCons y' VNil)
      where
        x' = c * x - s * y
        y' = s * x + c * y
        c = cos theta
        s = sin theta
    f _ = error "Unreachable" -- Can't find a type annotation for f that works
project (RotatePolar theta) = coerce f
  where
    f (VCons r (VCons theta' VNil)) = VCons r (VCons (theta + theta') VNil)
    f _ = error "Unreachable" -- GHC 7.10 can't deduce that this case is not required
project PlanarToPolar = coerce f
  where
    f :: (RealFloat a) => Vector '[DLength, DLength] a -> Vector '[DLength, DPlaneAngle] a
    f p@(VCons x (VCons y VNil)) = let r = length p
                                       theta = atan2 y x
                                    in (VCons r (VCons theta VNil))
    f _ = error "Unreachable" -- GHC 7.10 can't deduce that this case is not required
project PolarToPlanar = coerce f
  where
    f :: (Floating a) => Vector '[DLength, DPlaneAngle] a -> Vector '[DLength, DLength] a
    f (VCons r (VCons theta VNil)) = let x = r * cos theta
                                         y = r * sin theta
                                      in (VCons x (VCons y VNil))
    f _ = error "Unreachable" -- GHC 7.10 can't deduce that this case is not required

liftPoint :: (KnownCoordinateType (CoordinateSystemType b), Floating t, Ord t) => (Vector (Representation (CoordinateSystemType a)) t -> Vector (Representation (CoordinateSystemType b)) t) -> Point a t -> Point b t
liftPoint f (Point p) = point $ f p

{-

Offsets in cartesian coordinate systems form vector spaces.
Cartesian coordinate systems are affine spaces with differences represented as offsets.
Cartesian coordinate systems form metric spaces.

-}

instance (Fractional a, Real a) => VectorSpace (Offset ('CoordinateSystem sys 'Linear) a) where
  type Dimensions (Offset ('CoordinateSystem sys 'Linear) a) = Representation 'Linear
  fromList = fmap Offset . fromList
  toList (Offset v) = toList v
  zeroV = Offset zeroV
  (Offset x) ^+^ (Offset y) = Offset (x ^+^ y)
  negateV (Offset v) = Offset $ negateV v

instance (Fractional a, Real a) => MonoVectorSpace (Offset ('CoordinateSystem sys 'Linear) a) where
  type Element (Offset ('CoordinateSystem sys 'Linear) a) = a
  fromMonoList = fmap Offset . fromMonoList
  toMonoList (Offset v) = toMonoList v
  scale s (Offset v) = Offset $ scale s v

instance (Fractional a, Real a) => VectorSpace (Offset ('CoordinateSystem sys 'Planar) a) where
  type Dimensions (Offset ('CoordinateSystem sys 'Planar) a) = Representation 'Planar
  fromList = fmap Offset . fromList
  toList (Offset v) = toList v
  zeroV = Offset zeroV
  (Offset x) ^+^ (Offset y) = Offset (x ^+^ y)
  negateV (Offset v) = Offset $ negateV v

instance (Fractional a, Real a) => MonoVectorSpace (Offset ('CoordinateSystem sys 'Planar) a) where
  type Element (Offset ('CoordinateSystem sys 'Planar) a) = a
  fromMonoList = fmap Offset . fromMonoList
  toMonoList (Offset v) = toMonoList v
  scale s (Offset v) = Offset $ scale s v

instance (Fractional a, Real a) => VectorSpace (Offset ('CoordinateSystem sys 'Spatial) a) where
  type Dimensions (Offset ('CoordinateSystem sys 'Spatial) a) = Representation 'Spatial
  fromList = fmap Offset . fromList
  toList (Offset v) = toList v
  zeroV = Offset zeroV
  (Offset x) ^+^ (Offset y) = Offset (x ^+^ y)
  negateV (Offset v) = Offset $ negateV v

instance (Fractional a, Real a) => MonoVectorSpace (Offset ('CoordinateSystem sys 'Spatial) a) where
  type Element (Offset ('CoordinateSystem sys 'Spatial) a) = a
  fromMonoList = fmap Offset . fromMonoList
  toMonoList (Offset v) = toMonoList v
  scale s (Offset v) = Offset $ scale s v

instance (Fractional a, Real a) => AffineSpace (Point ('CoordinateSystem sys 'Linear) a) where
  type Diff (Point ('CoordinateSystem sys 'Linear) a) = Offset ('CoordinateSystem sys 'Linear) a
  (Point x) .-. (Point y) = Offset $ x ^-^ y
  (Point x) .+^ (Offset v) = Point $ x .+^ v

instance (Fractional a, Real a) => AffineSpace (Point ('CoordinateSystem sys 'Planar) a) where
  type Diff (Point ('CoordinateSystem sys 'Planar) a) = Offset ('CoordinateSystem sys 'Planar) a
  (Point x) .-. (Point y) = Offset $ x ^-^ y
  (Point x) .+^ (Offset v) = Point $ x .+^ v

instance (Fractional a, Real a) => AffineSpace (Point ('CoordinateSystem sys 'Spatial) a) where
  type Diff (Point ('CoordinateSystem sys 'Spatial) a) = Offset ('CoordinateSystem sys 'Spatial) a
  (Point x) .-. (Point y) = Offset $ x ^-^ y
  (Point x) .+^ (Offset v) = Point $ x .+^ v

instance MetricSpace (Point ('CoordinateSystem sys 'Linear)) where
  type DistanceDimension (Point ('CoordinateSystem sys 'Linear)) = DLength
  distance (Point x) (Point y) = distance x y

instance MetricSpace (Point ('CoordinateSystem sys 'Planar)) where
  type DistanceDimension (Point ('CoordinateSystem sys 'Planar)) = DLength
  distance (Point x) (Point y) = distance x y

instance MetricSpace (Point ('CoordinateSystem sys 'Spatial)) where
  type DistanceDimension (Point ('CoordinateSystem sys 'Spatial)) = DLength
  distance (Point x) (Point y) = distance x y

{-

Show instances for points and offsets.
These are split out by Representation to avoid a complicated constraint that amounts to all the dimensions being known.

-}

instance (Fractional a, Real a, Show a, KnownSymbol sys) => Show (Point ('CoordinateSystem sys 'Linear) a) where
  show (Point v) = symbolVal (Proxy :: Proxy sys) ++ " Point: " ++ show v

instance (Fractional a, Real a, Show a, KnownSymbol sys) => Show (Point ('CoordinateSystem sys 'Circular) a) where
  show (Point v) = symbolVal (Proxy :: Proxy sys) ++ " Point: " ++ show v

instance (Fractional a, Real a, Show a, KnownSymbol sys) => Show (Point ('CoordinateSystem sys 'Planar) a) where
  show (Point v) = symbolVal (Proxy :: Proxy sys) ++ " Point: " ++ show v  

instance (Fractional a, Real a, Show a, KnownSymbol sys) => Show (Point ('CoordinateSystem sys 'Polar) a) where
  show (Point v) = symbolVal (Proxy :: Proxy sys) ++ " Point: " ++ show v

instance (Fractional a, Real a, Show a, KnownSymbol sys) => Show (Point ('CoordinateSystem sys 'Spherical) a) where
  show (Point v) = symbolVal (Proxy :: Proxy sys) ++ " Point: " ++ show v

instance (Fractional a, Real a, Show a, KnownSymbol sys) => Show (Point ('CoordinateSystem sys 'Cylindrical) a) where
  show (Point v) = symbolVal (Proxy :: Proxy sys) ++ " Point: " ++ show v

instance (Fractional a, Real a, Show a, KnownSymbol sys) => Show (Point ('CoordinateSystem sys 'Spatial) a) where
  show (Point v) = symbolVal (Proxy :: Proxy sys) ++ " Point: " ++ show v

instance (Fractional a, Real a, Show a, KnownSymbol sys) => Show (Offset ('CoordinateSystem sys 'Linear) a) where
  show (Offset v) = symbolVal (Proxy :: Proxy sys) ++ " Offset: " ++ show v

instance (Fractional a, Real a, Show a, KnownSymbol sys) => Show (Offset ('CoordinateSystem sys 'Planar) a) where
  show (Offset v) = symbolVal (Proxy :: Proxy sys) ++ " Offset: " ++ show v

instance (Fractional a, Real a, Show a, KnownSymbol sys) => Show (Offset ('CoordinateSystem sys 'Spatial) a) where
  show (Offset v) = symbolVal (Proxy :: Proxy sys) ++ " Offset: " ++ show v
