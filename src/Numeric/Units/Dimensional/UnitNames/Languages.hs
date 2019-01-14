{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.Units.Dimensional.UnitNames.Languages
(
  Language(..)
, Optionality(..)
, weakenLanguage
, languageName
, ucum
, internationalEnglish
, internationalEnglishAbbreviation
, internationalEnglishAscii
, internationalEnglishAsciiAbbreviation
, usEnglish
, siunitx
)
where

import Prelude
import Control.DeepSeq (NFData(..))
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

data Optionality = Required 
                 | Optional
  deriving (Data, Typeable, Generic, Read, Show, Eq, Ord, NFData)

-- | A language of unit names, represented as an IETF language tag.
data Language (o :: Optionality) where
  OptionalLanguage :: String -> Language 'Optional
  RequiredLanguage :: String -> Language 'Required

deriving instance Eq (Language o)
deriving instance Ord (Language o)
deriving instance Show (Language o)
deriving instance (Typeable o) => Typeable (Language o)

instance NFData (Language o) where
  rnf = \case
          (OptionalLanguage x) -> rnf x
          (RequiredLanguage x) -> rnf x

weakenLanguage :: Language o -> Language Optional
weakenLanguage (RequiredLanguage x) = OptionalLanguage x
weakenLanguage x@(OptionalLanguage _) = x

languageName :: Language o -> String
languageName (OptionalLanguage l) = l
languageName (RequiredLanguage l) = l

-- | The language of unit names standardized by the Unified Code for Units of Measure.
--
-- See <http://unitsofmeasure.org/ here> for further information.
ucum :: Language 'Optional
ucum = OptionalLanguage "x-ucum"

-- | The language of unit names used by the siunitx LaTeX package.
--
-- See <https://www.ctan.org/pkg/siunitx here> for further information.
siunitx :: Language 'Optional
siunitx = OptionalLanguage "x-siunitx"

-- | The English language.
internationalEnglish :: Language 'Required
internationalEnglish = RequiredLanguage "en"

-- | The English language, restricted to the ASCII character set.
internationalEnglishAscii :: Language 'Optional
internationalEnglishAscii = OptionalLanguage "en-x-ascii"

-- | The English language, as used in the United States.
usEnglish :: Language 'Optional
usEnglish = OptionalLanguage "en-US"

-- | An abbreviation in the English language.
internationalEnglishAbbreviation :: Language 'Required
internationalEnglishAbbreviation = RequiredLanguage "en-x-abbrev"

-- | An abbreviation in the English language, restricted to the ASCII character set.
internationalEnglishAsciiAbbreviation :: Language 'Optional
internationalEnglishAsciiAbbreviation = OptionalLanguage "en-x-abbrev-ascii"
