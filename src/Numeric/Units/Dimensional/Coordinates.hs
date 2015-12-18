{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.Units.Dimensional.Coordinates where

import GHC.TypeLits (Symbol)
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.Vectors

data CoordinateType = Linear | Circular | Planar | Polar | Spherical | Cylindrical | Spatial -- what to name the 3D cartesian one is unclear

type family Representation (t :: CoordinateType) :: [Dimension] where
  Representation 'Linear = '[DLength]
  Representation 'Circular = '[DPlaneAngle]
  Representation 'Planar = '[DLength, DLength]
  Representation 'Polar = '[DLength, DPlaneAngle]
  Representation 'Spherical = '[DLength, DPlaneAngle, DPlaneAngle]
  Representation 'Cylindrical = '[DLength, DPlaneAngle, DLength]
  Representation 'Spatial = '[DLength, DLength, DLength]

newtype Point (sys :: Symbol) (ty :: CoordinateType) a = Point (Torsor (Vector (Representation ty) a))

type ECEF = Point "ECEF" 'Spatial

here, there :: ECEF Double
here = Point . Torsor $ VCons (3 *~ meter) (VCons (-12 *~ meter) (VCons (7 *~ meter) VNil))
there = Point . Torsor $ VCons (19.4 *~ meter) (VCons (171.9 *~ meter) (VCons (-41.6 *~ meter) VNil))

instance (Fractional a, Real a, Show a) => Show (ECEF a) where
  show (Point (Torsor v)) = show v

instance (Fractional a, Real a) => MetricSpace (Point sys 'Spatial a) where
  type DistanceDimension (Point sys 'Spatial a) = DLength
  distance (Point x) (Point y) = distance x y
