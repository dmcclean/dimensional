{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Numeric.Units.Dimensional.Presentation
(
  -- * Presentation Quantities
  PresentationQuantity(..)
  -- * Presentation Formats
, PresentationFormat(..)
, defaultFormat
)
where

import Data.Data
import GHC.Generics
import Numeric.Units.Dimensional

data PresentationQuantity d a = Simple a (Unit 'NonMetric d a)
                              | Composite Integer (Unit 'NonMetric d a) (PresentationQuantity d a)
  deriving (Eq, Generic, Generic1, Typeable)

data PresentationFormat d a = SimpleFormat (a -> String) (Unit 'NonMetric d a)
                            | CompositeFormat (Unit 'NonMetric d a) (PresentationFormat d a)

defaultFormat :: (Num a, Show a, KnownDimension d) => PresentationFormat d a
defaultFormat = SimpleFormat show siUnit
