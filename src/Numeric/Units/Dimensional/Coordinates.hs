{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.Units.Dimensional.Coordinates
(
  CoordinateType(..),
  CoordinateSystem(..),
  CoordinateSystemType,
  KnownCoordinateType, canonicalize,
  Projection(..), project, invert,
  Point(..), Offset(..), Direction(..), direction, offsetBy,
  here, there, doug, centerOfEarth
)
where

import Data.Coerce
import Data.Proxy
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.Vectors hiding (direction)
import qualified Numeric.Units.Dimensional.Vectors as V
import Unsafe.Coerce

data CoordinateType = Linear
                    | Planar
                    | Polar
                    | Spherical
                    | Cylindrical
                    | Spatial -- what to name the 3D cartesian one is unclear

data CoordinateSystem = CoordinateSystem Symbol CoordinateType

type family Representation (t :: CoordinateType) :: [Dimension] where
  Representation 'Linear = '[DLength]
  Representation 'Planar = '[DLength, DLength]
  Representation 'Polar = '[DLength, DPlaneAngle]
  Representation 'Spherical = '[DLength, DPlaneAngle, DPlaneAngle]
  Representation 'Cylindrical = '[DLength, DPlaneAngle, DLength]
  Representation 'Spatial = '[DLength, DLength, DLength]

class KnownCoordinateType (ty :: CoordinateType) where
  canonicalize :: (Ord a, Floating a) => Proxy ty -> Vector (Representation ty) a -> Vector (Representation ty) a

instance KnownCoordinateType 'Linear where
  canonicalize _ = id

instance KnownCoordinateType 'Planar where
  canonicalize _ = id

instance KnownCoordinateType 'Polar where
  canonicalize _ (V2 r theta) | r < _0    = V2 (negate r) (singleTurnAngle (theta + pi))
                              | otherwise = V2 r          (singleTurnAngle theta)
  canonicalize _ x = x -- GHC 7.10 can't realize that this case is unreachable

instance KnownCoordinateType 'Spherical where
  canonicalize _ (V3 r theta phi) = (V3 r theta phi) -- should constrain angles, non-negative radius
  canonicalize _ x = x -- GHC 7.10 can't realize that this case is unreachable

instance KnownCoordinateType 'Cylindrical where
  canonicalize _ (V3 r theta z) | r < _0    = V3 (negate r) (singleTurnAngle (theta + pi)) z
                                | otherwise = V3 r (singleTurnAngle theta) z
  canonicalize _ x = x -- GHC 7.10 can't realize that this case is unreachable

instance KnownCoordinateType 'Spatial where
  canonicalize _ = id

singleTurnAngle :: (Floating a) => PlaneAngle a -> PlaneAngle a
singleTurnAngle = id

type family CoordinateSystemType (sys :: CoordinateSystem) :: CoordinateType where
  CoordinateSystemType ('CoordinateSystem sys ty) = ty

newtype Point (sys :: CoordinateSystem) a = Point (Vector (Representation (CoordinateSystemType sys)) a)
  deriving (Eq)

point :: forall sys a.(KnownCoordinateType (CoordinateSystemType sys), Ord a, Floating a) => Vector (Representation (CoordinateSystemType sys)) a -> Point sys a
point = Point . (canonicalize (Proxy :: Proxy (CoordinateSystemType sys)))

newtype Offset (sys :: CoordinateSystem) a = Offset (Vector (Representation (CoordinateSystemType sys)) a)
  deriving (Eq)

newtype Direction (sys :: CoordinateSystem) a = Direction { unDirection :: DirectionVector (Offset sys) a }
  deriving (Eq)

type ECEF = 'CoordinateSystem "ECEF" 'Spatial

here, there, doug, centerOfEarth :: Point ECEF Double
here = point $ V3 (3 *~ meter) (-12 *~ meter) (7 *~ meter)
there = point $ V3 (19.4 *~ meter) (171.9 *~ meter) (-41.6 *~ meter)
doug = point $ V3 (1557.123 *~ kilo meter) (-4471.044 *~ kilo meter) (4259.591 *~ kilo meter)
centerOfEarth = point zeroV

direction :: (Real a, Floating a, MetricSpace (Offset sys), DistanceDimension (Offset sys) ~ HomogenousDimension (Dimensions (Offset sys a)), VectorSpace (Offset sys a), VectorSpace (Vector (Dimensions (Offset sys a)) a)) => Offset sys a -> Direction sys a
direction o = Direction (V.direction o)

offsetBy :: (Num a) => Direction sys a -> Length a -> Offset sys a
offsetBy dir dist = Offset . unsafeCoerce . gscale dist . unUnitV . unDirection $ dir
  -- this unsafeCoerce wouldn't be necessary if we could have appropriate role signatures in the definition of Dimensional

{-

Projection between coordinate systems

-}

data Projection (t :: *) (a :: CoordinateSystem) (b :: CoordinateSystem) where
  Identity :: (CoordinateSystemType a ~ CoordinateSystemType b) => Projection t a b
  Opaque :: (Point a t -> Point b t) -> Maybe (Point b t -> Point a t) -> Projection t a b
  Composed :: Projection t b c -> Projection t a b -> Projection t a c
  Translate :: (AffineSpace (Point sys t), Offset sys t ~ Diff (Point sys t)) => Offset sys t -> Projection t sys sys
  RotatePlanar :: (Real t, Floating t) => PlaneAngle t -> Projection t ('CoordinateSystem a 'Planar) ('CoordinateSystem b 'Planar)
  RotatePolar :: (Real t, Fractional t) => PlaneAngle t -> Projection t ('CoordinateSystem a 'Polar) ('CoordinateSystem b 'Polar)
  PlanarToPolar :: (RealFloat t) => Projection t ('CoordinateSystem a 'Planar) ('CoordinateSystem b 'Polar)
  PolarToPlanar :: (RealFloat t) => Projection t ('CoordinateSystem a 'Polar) ('CoordinateSystem b 'Planar) -- technically possible under a weaker constraint but loses invertibility

instance Category (Projection t) where
  id = Identity
  (.) = Composed -- there are many opportunities for optimization here, eg merging consecutive projection matrices

invert :: Projection t a b -> Maybe (Projection t b a)
invert Identity = Just Identity
invert (Opaque f (Just r)) = Just $ Opaque r (Just f)
invert (Opaque _ _) = Nothing
invert (Composed bc ab) = do
                            cb <- invert bc
                            ba <- invert ab
                            return $ Composed ba cb
invert (Translate offset)     = Just $ Translate      (negateV offset)
invert (RotatePlanar angle)   = Just $ RotatePlanar   (negate angle)
invert (RotatePolar angle)    = Just $ RotatePolar    (negate angle)
invert PlanarToPolar = Just PolarToPlanar
invert PolarToPlanar = Just PlanarToPolar

project :: Projection t a b -> Point a t -> Point b t
project Identity = coerce
project (Opaque f _) = f
project (Composed bc ab) = project bc . project ab
project (Translate offset) = (.+^ offset)
project (RotatePlanar theta) = coerce f
  where
    f (V2 x y) = V2 x' y'
      where
        x' = c * x - s * y
        y' = s * x + c * y
        c = cos theta
        s = sin theta
    f _ = error "Unreachable" -- Can't find a type annotation for f that works
project (RotatePolar theta) = coerce f
  where
    f (V2 r theta') = V2 r (theta + theta')
    f _ = error "Unreachable" -- GHC 7.10 can't deduce that this case is not required
project PlanarToPolar = coerce f
  where
    f :: (RealFloat a) => Vector '[DLength, DLength] a -> Vector '[DLength, DPlaneAngle] a
    f p@(V2 x y) = let r = lengthV p
                       theta = atan2 y x
                    in V2 r theta
    f _ = error "Unreachable" -- GHC 7.10 can't deduce that this case is not required
project PolarToPlanar = coerce f
  where
    f :: (Floating a) => Vector '[DLength, DPlaneAngle] a -> Vector '[DLength, DLength] a
    f (V2 r theta) = let x = r * cos theta
                         y = r * sin theta
                      in V2 x y
    f _ = error "Unreachable" -- GHC 7.10 can't deduce that this case is not required

{-

Offsets in cartesian coordinate systems form vector spaces.
Cartesian coordinate systems are affine spaces with differences represented as offsets.
Cartesian coordinate systems form metric spaces.

-}

fmapOverFirst :: (Functor f) => (a -> b) -> (f a, x) -> (f b, x)
fmapOverFirst f (x, y) = (fmap f x, y)

instance (Fractional a, Real a, CVectorMono (Vector (Representation ty) a)) => CVectorMono (Point ('CoordinateSystem sys ty) a) where
  type Dimensions (Point ('CoordinateSystem sys ty) a) = Representation ty
  fromListWithLeftovers = fmapOverFirst Point . fromListWithLeftovers
  toList (Point v) = toList v

instance (Fractional a, Real a, CVectorMono (Vector (Representation ty) a)) => CVectorMono (Offset ('CoordinateSystem sys ty) a) where
  type Dimensions (Offset ('CoordinateSystem sys ty) a) = Representation ty
  fromListWithLeftovers = fmapOverFirst Offset . fromListWithLeftovers
  toList (Offset v) = toList v

deriving instance (CVector (Vector (Representation ty))) => CVector (Point ('CoordinateSystem sys ty))

deriving instance (CVector (Vector (Representation ty))) => CVector (Offset ('CoordinateSystem sys ty))

deriving instance (Fractional a, Real a) => VectorSpace (Offset ('CoordinateSystem sys 'Linear) a)

deriving instance MonoVectorSpace (Offset ('CoordinateSystem sys 'Linear))

deriving instance (Fractional a, Real a) => VectorSpace (Offset ('CoordinateSystem sys 'Planar) a)

deriving instance MonoVectorSpace (Offset ('CoordinateSystem sys 'Planar))

deriving instance (Fractional a, Real a) => VectorSpace (Offset ('CoordinateSystem sys 'Spatial) a)

deriving instance MonoVectorSpace (Offset ('CoordinateSystem sys 'Spatial))

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

instance MetricSpace (Offset ('CoordinateSystem sys 'Linear)) where
  type DistanceDimension (Offset ('CoordinateSystem sys 'Linear)) = DLength
  distance (Offset x) (Offset y) = distance x y

instance MetricSpace (Point ('CoordinateSystem sys 'Planar)) where
  type DistanceDimension (Point ('CoordinateSystem sys 'Planar)) = DLength
  distance (Point x) (Point y) = distance x y

instance MetricSpace (Offset ('CoordinateSystem sys 'Planar)) where
  type DistanceDimension (Offset ('CoordinateSystem sys 'Planar)) = DLength
  distance (Offset x) (Offset y) = distance x y

instance MetricSpace (Point ('CoordinateSystem sys 'Spatial)) where
  type DistanceDimension (Point ('CoordinateSystem sys 'Spatial)) = DLength
  distance (Point x) (Point y) = distance x y

instance MetricSpace (Offset ('CoordinateSystem sys 'Spatial)) where
  type DistanceDimension (Offset ('CoordinateSystem sys 'Spatial)) = DLength
  distance (Offset x) (Offset y) = distance x y

{-

Show instances for points, offsets, and directions.
These are split out by Representation to avoid a complicated constraint that amounts to all the dimensions being known.

-}

instance (Fractional a, Real a, Show a, KnownSymbol sys) => Show (Point ('CoordinateSystem sys 'Linear) a) where
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

instance (Fractional a, Real a, Show a, KnownSymbol sys) => Show (Direction ('CoordinateSystem sys 'Linear) a) where
  show (Direction v) = symbolVal (Proxy :: Proxy sys) ++ " Direction: " ++ show v

instance (Fractional a, Real a, Show a, KnownSymbol sys) => Show (Direction ('CoordinateSystem sys 'Planar) a) where
  show (Direction v) = symbolVal (Proxy :: Proxy sys) ++ " Direction: " ++ show v

instance (Fractional a, Real a, Show a, KnownSymbol sys) => Show (Direction ('CoordinateSystem sys 'Spatial) a) where
  show (Direction v) = symbolVal (Proxy :: Proxy sys) ++ " Direction: " ++ show v
