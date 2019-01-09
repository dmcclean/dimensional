{-# LANGUAGE DataKinds #-}

module Numeric.Units.Dimensional.UnitNamesSpec where

import Numeric.Units.Dimensional.UnitNames
import Numeric.Units.Dimensional.UnitNames.Internal (UnitName'(..), eliminateOnes, eliminateGrouping, eliminateRedundantPowers, distributePowers)
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
            describe "with eliminateOnes" $ do
              it "properly eliminates redundant One from products" $ do
                let n = name' $ meter D.* one D.* D.kilo gram D.* one
                let n' = name' $ meter D.* D.kilo gram
                eliminateOnes n `shouldBe` n'
              it "properly eliminates redundant One from denominator" $ do
                let n = name' $ meter D./ (one D.* one)
                let n' = name'' meter
                eliminateOnes n `shouldBe` n'
              it "properly eliminates powers of One" $ do
                let n = name' $ meter D.* one D.^ pos3 D.* liter D./ (one D.^ neg1)
                let n' = name' $ meter D.* liter
                eliminateOnes n `shouldBe` n'
              it "properly eliminates grouped appearances of One" $ do
                let n = name' $ D.grouped (one D.* meter)
                let n' = name' $ D.grouped meter
                eliminateOnes n `shouldBe` n'
            describe "with eliminateGrouping" $ do
              it "eliminates grouping" $ do
                let n = name' $ D.grouped $ D.grouped meter
                let n' = name'' meter
                eliminateGrouping n `shouldBe` n'
              it "eliminates grouping under products" $ do
                let u' = mole D./ liter
                let u = D.grouped $ D.grouped u'
                let n = name' $ u D.* (u D./ u)
                let n' = name' $ u' D.* (u' D./ u')
                eliminateGrouping n `shouldBe` n'
            describe "with eliminateRedundantPowers" $ do
              it "eliminates zero exponents" $ do
                let n = name' $ ampere D.* meter D.^ zero
                let n' = name' $ ampere D.* one
                eliminateRedundantPowers n `shouldBe`n'
              it "eliminates one exponents" $ do
                let n = name' $ ampere D./ meter D.^ pos1
                let n' = name' $ ampere D./ meter
                eliminateRedundantPowers n `shouldBe`n'
              it "eliminates exponents of One" $ do
                let n = name' $ ampere D.* one D.^ pos3
                let n' = name' $ ampere D.* one
                eliminateRedundantPowers n `shouldBe`n'
              it "eliminates nested exponents" $ do
                let n = name' $ (meter D.^ neg1) D.^ pos2 D.* kilo gram
                let n' = name' $ meter D.^ neg2 D.* kilo gram
                eliminateRedundantPowers n `shouldBe`n'
              it "preserves other exponents" $ do
                let n = name' $ ampere D./ meter D.^ pos2
                eliminateRedundantPowers n `shouldBe`n
            describe "with distributePowers" $ do
              it "distributes powers to molecules" $ do
                let n = name' $ liter D./ D.grouped ((kilo gram D.* meter) D.^ pos2)
                let n' = name' $ liter D./ D.grouped (kilo gram D.^pos2 D.* meter D.^ pos2)
                distributePowers n `shouldBe` n'
          describe "Unit name formatting" $ do
            it "formats atomic unit names" $ do
              let n = name' ampere
              show n `shouldBe` "A"
            it "formats prefixed unit names" $ do
              let n = name' $ kilo watt
              show n `shouldBe` "kW"
            it "formats product unit names" $ do
              let n = name' $ newton D.* meter
              show n `shouldBe` "N\xA0m"
            it "formats simple quotient unit names" $ do
              let n = name' $ meter D./ second
              show n `shouldBe` "m\xA0/\xA0s"
            it "formats complex quotient unit names" $ do
              let n = name' $ volt D./ (meter D./ second)
              show n `shouldBe` "V\xA0/\xA0(m\xA0/\xA0s)"
            it "formats simple power unit names" $ do
              let n = name' $ square meter
              show n `shouldBe` "m^2"
            it "formats prefixed power unit names" $ do
              let n = name' $ square $ kilo meter
              show n `shouldBe` "km^2"
            it "formats complex power unit names" $ do
              let n = name' $ watt D.* (meter D.* kelvin) D.^ neg1
              show n `shouldBe` "W\xA0(m\xA0K)^-1"
            it "formats grouped unit names" $ do
              let n = name' $ kilo gram D.* (D.grouped $ meter D./ second)
              show n `shouldBe` "kg\xA0(m\xA0/\xA0s)"
