{-# LANGUAGE TupleSections #-}

module Numeric.Units.Dimensional.DK.Dimensions.OptimalFactoring
where

import Data.Foldable
import Data.Ord
import Data.Proxy
import Numeric.Units.Dimensional.DK.Dimensions.TermLevel
import Numeric.Units.Dimensional.DK.Quantities
import Prelude hiding ((/), (*), recip)
import qualified Prelude as P

type Cost = Double

type Step = (Dimension', Cost)
type Path = ([Dimension'], Cost)

newtype CostMap = CostMap [Step]

defaultCostMap :: CostMap
defaultCostMap = CostMap . fmap ((, 1)) $ [dLength, dMass, dTime, dElectricCurrent, dThermodynamicTemperature, dAmountOfSubstance, dLuminousIntensity]

insert :: Step -> CostMap -> CostMap
insert s (CostMap cs) = CostMap (s : cs) -- todo: validate it

siCostMap = id
          $ insert (dimension (Proxy :: Proxy DPower), 0.5)
          $ insert (dimension (Proxy :: Proxy DElectricPotential), 0.5)
          $ defaultCostMap

minCost :: CostMap -> Dimension' -> [Dimension']
minCost (CostMap gs) d = fst $ minCost' d gs' emptyPath
  where
    gs' = filter (relevant d) gs

minCost' :: Dimension' -> [Step] -> Path -> Path
minCost' d [] _ | d == dOne = emptyPath
                | otherwise = ([d], 1 P./ 0) -- we really shouldn't reach here because we have all the generators as possible steps to start with
minCost' d gs p = leastCostly [minCost' (d / ds) (filter (relevant (d / ds)) gs) (extend p s) | s@(ds, _) <- possibleSteps gs]
  where
    possibleSteps :: [Step] -> [Step]
    possibleSteps [] = []
    possibleSteps ((s, cs) : steps) = (s, cs) : (recip s, cs) : possibleSteps steps

emptyPath :: Path
emptyPath = ([], 0)

extend :: Path -> Step -> Path
extend (p, cp) (s, cs) = (s:p, cp + cs)

relevant :: Dimension' -> Step -> Bool
relevant d1 (d2, _) = relevantD d1 d2

relevantD :: Dimension' -> Dimension' -> Bool
relevantD d1 d2 = any id $ zipWith (&&) (fmap (/= 0) $ asList d1) (fmap (/= 0) $ asList d2)

naiveCost :: Dimension' -> Cost
naiveCost = fromIntegral . sum . asList

leastCostly :: [(a, Cost)] -> (a, Cost)
leastCostly = minimumBy (comparing snd)
