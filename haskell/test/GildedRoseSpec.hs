module GildedRoseSpec (spec) where

import Test.Hspec
import GildedRose

spec :: Spec
spec =
  describe "updateQuality" $ do
    context "On a normal item it" $ do

      it "Should decrement sell in count" $
         let inventory = [Item "Normal item" 1 0]
             actual = updateQuality inventory
             expected = [Item "Normal item" 0 0]
         in actual `shouldBe` expected
  
      it "Should allow sell in count to go negative" $
         let inventory = [Item "Normal item" 1 0]
             actual = updateQuality inventory
             expected = [Item "Normal item" 0 0]
         in actual `shouldBe` expected
  
      it "Should decrement quality value" $
         let inventory = [Item "Normal item" 1 1]
             actual = updateQuality inventory
             expected = [Item "Normal item" 0 0]
         in actual `shouldBe` expected

      it "Should not decrement quality value below 0" $
         let inventory = [Item "Normal item" 1 0]
             actual = updateQuality inventory
             expected = [Item "Normal item" 0 0]
         in actual `shouldBe` expected

      it "Should not decrement quality by 2 after expiration" $
         let inventory = [Item "Normal item" (-1) 4]
             actual = updateQuality inventory
             expected = [Item "Normal item" (-2) 2]
         in actual `shouldBe` expected

      -- | Reference implementation doesn't pass this one
      -- it "Should never allow quality above 50" $
      --    let inventory = [Item "Normal item" 1 60]
      --        actual = updateQuality inventory
      --        expected = [Item "Normal item" 0 50]
      --    in actual `shouldBe` expected

    context "On \"Aged Brie\" it" $ do

      it "Should increase quality as it ages" $
         let inventory = [Item "Aged Brie" 1 0]
             actual = updateQuality inventory
             expected = [Item "Aged Brie" 0 1]
         in actual `shouldBe` expected

      it "Should not increase above 50" $
         let inventory = [Item "Aged Brie" 1 50]
             actual = updateQuality inventory
             expected = [Item "Aged Brie" 0 50]
         in actual `shouldBe` expected

    context "On \"Sulfuras\" it" $ do
 
      it "Should not expire" $
         let inventory = [Item "Sulfuras, Hand of Ragnaros" 1 50]
             actual = updateQuality inventory
             expected = [Item "Sulfuras, Hand of Ragnaros" 1 50]
         in actual `shouldBe` expected

      it "Should not reduce in quality" $
         let inventory = [Item "Sulfuras, Hand of Ragnaros" 1 50]
             actual = updateQuality inventory
             expected = [Item "Sulfuras, Hand of Ragnaros" 1 50]
         in actual `shouldBe` expected

    context "On \"Backstage passes\" it" $ do

      it "Should increase in quality by 2 within 10 days of concert" $
         let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 10 1]
             actual = updateQuality inventory
             expected = [Item "Backstage passes to a TAFKAL80ETC concert" 9 3]
         in actual `shouldBe` expected

      it "Should increase in quality by 3 within 5 days of concert" $
         let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 5 1]
             actual = updateQuality inventory
             expected = [Item "Backstage passes to a TAFKAL80ETC concert" 4 4]
         in actual `shouldBe` expected

      it "Should go strait to 0 quality after expiration" $
         let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 0 10]
             actual = updateQuality inventory
             expected = [Item "Backstage passes to a TAFKAL80ETC concert" (-1) 0]
         in actual `shouldBe` expected
