{-# LANGUAGE DataKinds #-}

module Numeric.Units.Dimensional.UnitNamesSpec where

import Numeric.Units.Dimensional.UnitNames
import Numeric.Units.Dimensional.UnitNames.Internal (UnitName'(..), eliminateOnes)
import Numeric.Units.Dimensional.UnitNames.Atoms (atom)
import Numeric.Units.Dimensional.UnitNames.Languages
import Numeric.Units.Dimensional.Prelude hiding ((*), (/), product, weaken)
import qualified Numeric.Units.Dimensional.Prelude as D
import Test.Hspec

-- used to avoid type ambiguity
name' :: Unit m d Double -> UnitName m
name' = name

-- used to avoid type ambiguity and weaken
name'' :: Unit m d Double -> UnitName 'NonMetric
name'' = weaken . name'

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
            let nMeter = name' meter
            let nMeter' = weaken nMeter
            let nKilogram = name' $ kilo gram
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
          describe "UnitName simplification" $ do
            it "properly eliminates redundant One from products" $ do
              let n = name' $ (meter D.* one D.* D.kilo gram D.* one)
              let n' = name' $ (meter D.* D.kilo gram)
              eliminateOnes n `shouldBe` n'
            it "properly eliminates redundant One from denominator" $ do
              let n = name' $ meter D./ (one D.* one)
              let n' = name'' meter
              eliminateOnes n `shouldBe` n'
