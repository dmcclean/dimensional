module Numeric.Units.Dimensional.PresentationSpec where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.NonSI
import Numeric.Units.Dimensional.Presentation
import Test.Hspec

spec :: Spec
spec = do
         describe "Quantity presentation" $ do
           let x = 12.7 *~ meter :: Length Double
           context "Decimal simple units" $ do
             let f = \d u -> SimpleFormat (DecimalFormat d) (simpleUnit u)
             it "renders correctly with zero decimals" $ do
                show (presentIn (f 0 meter) x) `shouldBe` "13 m"
             it "renders correctly with three decimals" $ do
                show (presentIn (f 3 meter) x) `shouldBe` "12.700 m"
             it "renders correctly with unit conversion" $ do
                show (presentIn (f 2 foot) x) `shouldBe` "41.67 ft"
             it "renders correctly with no integer part" $ do
                show (presentIn (f 3 (kilo meter)) x) `shouldBe` "0.013 km"
           context "Decimal composite units" $ do
             let f = \d major minor -> CompositeFormat major (SimpleFormat (DecimalFormat d) (simpleUnit minor)) 
             it "renders correctly with zero decimals" $ do
               show (presentIn (f 0 foot inch) x) `shouldBe` "41 ft 8 in"
             it "renders correctly with three decimals" $ do
               show (presentIn (f 3 foot inch) x) `shouldBe` "41 ft 8.000 in"
             it "renders correctly with negative quantities" $ do
               show (presentIn (f 3 foot inch) (negate x)) `shouldBe` "-41 ft 8.000 in"
             it "renders correctly with zero major part" $ do
               show (presentIn (f 3 foot inch) (6.3 *~ inch)) `shouldBe` "0 ft 6.300 in"
             it "renders correctly with zero minor part" $ do
               show (presentIn (f 3 foot inch) (2 *~ foot)) `shouldBe` "2 ft 0.000 in"
           context "Decimal prefixed units" $ do
             let f = \d u -> SimpleFormat (DecimalFormat d) (majorSiPrefixedUnit u)
             it "renders correctly with no prefix" $ do
               show (presentIn (f 3 meter) x) `shouldBe` "12.700 m"
             it "renders correctly with positive prefix" $ do
               show (presentIn (f 3 watt) (47.2 *~ kilo watt)) `shouldBe` "47.200 kW"
             it "renders correctly with negative prefix" $ do
               show (presentIn (f 3 meter) (3 *~ mil)) `shouldBe` "76.200 μm"
               show (presentIn (f 3 meter) (0.3 *~ mil)) `shouldBe` "7.620 μm"
               show (presentIn (f 3 meter) (0.03 *~ mil)) `shouldBe` "762.000 nm"

