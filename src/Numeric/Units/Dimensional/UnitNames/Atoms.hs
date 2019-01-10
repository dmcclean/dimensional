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
, requiredNameComponent
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

type AnyLanguage = String

-- | Represents the name of an atomic unit or prefix.
newtype NameAtom
  = NameAtom (M.Map AnyLanguage String) -- It's an invariant that internationalEnglish and internationalEnglishAbbreviation must appear as keys in the map. If the 'NameAtomType' is a prefix or metric, it must also contain 'ucum' as a key.
  deriving (Eq, Data, Typeable, Generic, NFData)

instance Ord NameAtom where
  compare = comparing $ requiredNameComponent internationalEnglishAbbreviation

-- | Constructs a 'NameAtom' of some 'NameAtomType'.
atom :: String -- ^ Abbreviated name in international English
     -> String -- ^ Full name in international English
     -> [(Language 'Optional, String)] -- ^ List of names in other 'Language's.
     -> NameAtom
atom a f ns = NameAtom $ M.union (M.mapKeys languageName (M.fromList ns)) ascii'
  where
    ascii' = if (isValidAscii f) then M.insert (languageName internationalEnglishAscii) f ascii else ascii
    ascii = if (isValidAscii a) then M.insert (languageName internationalEnglishAsciiAbbreviation) a basic else basic
    basic = M.fromList [(languageName internationalEnglishAbbreviation, a), (languageName internationalEnglish, f)]
    isValidAscii :: String -> Bool
    isValidAscii = all (\c -> C.isAscii c && C.isPrint c)

nameComponent :: Language o -> NameAtom -> Maybe String
nameComponent l (NameAtom m) = M.lookup (languageName l) m

requiredNameComponent :: Language 'Required -> NameAtom -> String
requiredNameComponent l (NameAtom m) = m M.! (languageName l)
