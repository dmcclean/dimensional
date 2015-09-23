{-# LANGUAGE TupleSections #-}

module Numeric.Units.Dimensional.DK.Dimensions.OptimalFactoring
where

import Data.Foldable
import Data.List (sortBy)
import Data.Ord
import Data.Proxy
import Numeric.Units.Dimensional.DK.Dimensions.TermLevel
import Numeric.Units.Dimensional.DK.Quantities
import Prelude hiding ((/), (*), recip, any, sum)
import qualified Prelude as P

type Cost = Double

type Step = (Dimension', Cost)
type Path = ([Dimension'], Cost)

newtype CostMap = CostMap [Step]

defaultCostMap :: CostMap
defaultCostMap = CostMap . fmap ((, 1)) $ [dLength, dMass, dTime, dElectricCurrent, dThermodynamicTemperature, dAmountOfSubstance, dLuminousIntensity]

insert :: Step -> CostMap -> CostMap
insert s (CostMap cs) = CostMap (s : cs) -- todo: validate it

siCostMap :: CostMap
siCostMap = id
          $ insert (dimension (Proxy :: Proxy DPower), 0.5)
          $ insert (dimension (Proxy :: Proxy DElectricPotential), 0.5)
          $ defaultCostMap

minCost :: CostMap -> Dimension' -> Path
minCost (CostMap gs) d = minCost' d gs'' emptyPath (naiveCost d)
  where
    gs'' = sortBy (comparing fst) gs'
    gs' = gs ++ fmap (\(s,c) -> (recip s,c)) gs

minCost' :: Dimension' -> [Step] -> Path -> Cost -> Path
minCost' d _  p@(_, c) lim | d == dOne = p
                           | c >= lim  = ([d], 1 P./ 0)
minCost' d [] _        _   = ([d], 1 P./ 0) -- we really shouldn't reach here because we have all the generators as possible steps to start with
minCost' d gs p        lim = leastCostly [minCost' (d / ds) (retainRelevant ds gs) (extend p s) lim | s@(ds, _) <- gs]
  where
    retainRelevant ds = filter (\(ds',_) -> ds' /= recip ds && ds <= ds')

emptyPath :: Path
emptyPath = ([], 0)

extend :: Path -> Step -> Path
extend (p, cp) (s, cs) = (s:p, cp + cs)

relevant :: Dimension' -> Dimension' -> Bool
relevant d1 d2 = any id $ zipWith (&&) (fmap (/= 0) $ asList d1) (fmap (/= 0) $ asList d2)

naiveCost :: Dimension' -> Cost
naiveCost = fromIntegral . sum . fmap abs . asList

leastCostly :: [(a, Cost)] -> (a, Cost)
leastCostly = minimumBy (comparing snd)
