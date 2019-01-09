{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE KindSignatures #-}

module Numeric.Units.Dimensional.UnitNames.Atoms
(
  NameAtom
, PrefixName
, atom
, nameComponent
, definiteNameComponent
)
where

import Prelude
import Control.DeepSeq (NFData)
import qualified Data.Char as C
import Data.Data (Data, Typeable)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import GHC.Generics (Generic)
import Numeric.Units.Dimensional.UnitNames.Languages

-- | The name of a metric prefix.
type PrefixName = NameAtom

-- | Represents the name of an atomic unit or prefix.
newtype NameAtom
  = NameAtom (M.Map Language String) -- It's an invariant that internationalEnglish and internationalEnglishAbbreviation must appear as keys in the map. If the 'NameAtomType' is a prefix or metric, it must also contain 'ucum' as a key.
  deriving (Eq, Data, Typeable, Generic, NFData)

instance Ord NameAtom where
  compare = comparing $ definiteNameComponent internationalEnglishAbbreviation

-- | Constructs a 'NameAtom' of some 'NameAtomType'.
atom :: String -- ^ Abbreviated name in international English
     -> String -- ^ Full name in international English
     -> [(Language, String)] -- ^ List of names in other 'Language's.
     -> NameAtom
atom a f ns = NameAtom $ M.union (M.fromList ns) ascii'
  where
    ascii' = if (isValidAscii f) then M.insert internationalEnglishAscii f ascii else ascii
    ascii = if (isValidAscii a) then M.insert internationalEnglishAsciiAbbreviation a basic else basic
    basic = M.fromList [(internationalEnglishAbbreviation, a), (internationalEnglish, f)]
    isValidAscii :: String -> Bool
    isValidAscii = all (\c -> C.isAscii c && C.isPrint c)

definiteNameComponent :: Language -> NameAtom -> String
definiteNameComponent l (NameAtom m) = m M.! l

nameComponent :: Language -> NameAtom -> Maybe String
nameComponent l (NameAtom m) = M.lookup l m
