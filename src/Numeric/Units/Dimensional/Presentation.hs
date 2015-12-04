{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Numeric.Units.Dimensional.Presentation

where

import Data.Data
import GHC.Generics
import Numeric.Units.Dimensional

data PresentationQuantity d a = Simple a (Unit 'NonMetric d a)
                              | Composite Integer (Unit 'NonMetric d a) (PresentationQuantity d a)
  deriving (Eq, Generic, Generic1, Typeable)


