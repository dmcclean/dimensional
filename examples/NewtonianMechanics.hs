{-# LANGUAGE NoImplicitPrelude #-}
module NewtonianMechanics where

import Numeric.Units.Dimensional.DK.Prelude

translationalKineticEnergy :: (Fractional a) => Mass a -> Velocity a -> Energy a
translationalKineticEnergy m v = m * v ^ pos2 / _2

translationalWork :: (Num a) => Force a -> Length a -> Energy a
translationalWork f d = f * d

translationalMomentum :: (Num a) => Mass a -> Velocity a -> Momentum a
translationalMomentum m v = m * v

translationalPower :: (Num a) => Force a -> Velocity a -> Power a
translationalPower f v = f * v

forceFromChangeInMomentum :: (Fractional a) => Momentum a -> Time a -> Force a
forceFromChangeInMomentum dp dt = dp / dt

rotationalKineticEnergy :: (Fractional a) => MomentOfInertia a -> AngularVelocity a -> Energy a
rotationalKineticEnergy i w = i * w ^ pos2 / (2 *~ radian ^ pos2)

rotationalWork :: (Fractional a) => Torque a -> PlaneAngle a -> Energy a
rotationalWork t th = t * th / (1 *~ radian)

rotationalMomentum :: (Fractional a) => MomentOfInertia a -> AngularVelocity a -> AngularMomentum a
rotationalMomentum i w = i * w / (1 *~ radian)

rotationalPower :: (Fractional a) => Torque a -> AngularVelocity a -> Power a
rotationalPower t w = t * w / (1 *~ radian)

torque :: (Fractional a) => Force a -> Length a -> Torque a
torque r f = r * f

torqueFromChangeInMomentum :: (Fractional a) => AngularMomentum a -> Time a -> Torque a
torqueFromChangeInMomentum dL dt = dL / dt
