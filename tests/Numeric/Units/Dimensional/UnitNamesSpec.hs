{-# LANGUAGE DataKinds #-}

module Numeric.Units.Dimensional.UnitNamesSpec where

import Numeric.Units.Dimensional.UnitNames
import Numeric.Units.Dimensional.UnitNames.Internal (UnitName'(..))
import Numeric.Units.Dimensional.UnitNames.Atoms (atom)
import Numeric.Units.Dimensional.UnitNames.Languages
import Numeric.Units.Dimensional.Prelude hiding ((*), (/), product, weaken)
import Test.Hspec

spec :: Spec
spec = do
          describe "NameAtom construction and formatting" $ do
            it "constructs NameAtoms without special languages" $ do
              let a = atom "m" "meter" []
              nameComponent internationalEnglish a `shouldBe` Just "meter"
              nameComponent internationalEnglishAbbreviation a `shouldBe` Just "m"
              nameComponent internationalEnglishAscii a `shouldBe` Just "meter"
              nameComponent ucum a `shouldBe` Nothing
            it "constructs NameAtoms with special languages" $ do
              let a = atom "m" "meter" [(ucum, "m")]
              nameComponent internationalEnglish a `shouldBe` Just "meter"
              nameComponent ucum a `shouldBe` Just "m"
            it "constructs NameAtoms with non-ASCII names" $ do
              let a = atom "Ω" "ohm" []
              nameComponent internationalEnglishAbbreviation a `shouldBe` Just "Ω"
              nameComponent internationalEnglishAsciiAbbreviation a `shouldBe` Nothing
          describe "UnitName arithmetic" $ do
            let nMeter = name $ (meter :: Unit 'Metric DLength Double)
            let nMeter' = weaken nMeter
            let nKilogram = name (kilo gram :: Unit 'NonMetric DMass Double)
            it "properly forms nullary products" $
              product [] `shouldBe` (One :: UnitName 'NonMetric)
            it "properly forms unary products" $
              product [nMeter'] `shouldBe` nMeter'
            it "properly forms binary products" $
              product [nMeter', nMeter'] `shouldBe` (Product nMeter' nMeter')
            it "right-associates ternary products" $
              product [nMeter', nMeter', nMeter'] `shouldBe` (Product nMeter' (Product nMeter' nMeter'))
            it "properly forms quotients" $
              (nMeter / nKilogram) `shouldBe` (Quotient nMeter' nKilogram) -- note the weakening
