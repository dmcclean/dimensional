{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Numeric.Units.Dimensional.UnitNames.Languages
(
  Language(..)
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
import Control.DeepSeq (NFData)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

-- | A language of unit names, represented as an IETF language tag.
newtype Language = Language String
  deriving (Read, Show, Eq, Ord, Generic, Typeable, Data, NFData)

-- | The language of unit names standardized by the Unified Code for Units of Measure.
--
-- See <http://unitsofmeasure.org/ here> for further information.
ucum :: Language
ucum = Language "x-ucum"

-- | The language of unit names used by the siunitx LaTeX package.
--
-- See <https://www.ctan.org/pkg/siunitx here> for further information.
siunitx :: Language
siunitx = Language "x-siunitx"

-- | The English language.
internationalEnglish :: Language
internationalEnglish = Language "en"

-- | The English language, restricted to the ASCII character set.
internationalEnglishAscii :: Language
internationalEnglishAscii = Language "en-x-ascii"

-- | The English language, as used in the United States.
usEnglish :: Language
usEnglish = Language "en-US"

-- | An abbreviation in the English language.
internationalEnglishAbbreviation :: Language
internationalEnglishAbbreviation = Language "en-x-abbrev"

-- | An abbreviation in the English language, restricted to the ASCII character set.
internationalEnglishAsciiAbbreviation :: Language
internationalEnglishAsciiAbbreviation = Language "en-x-abbrev-ascii"
