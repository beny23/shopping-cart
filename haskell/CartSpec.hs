module CartSpec where

import Cart
import qualified Data.Map.Strict as M
import Test.Hspec

main :: IO ()
main = hspec $ do
  let cornflakes = ("Cornflakes", 2.52)
  let initialCart = add 2 cornflakes empty

  describe "empty" $ do
    it "should create an empty cart" $ do
      empty `shouldBe` M.empty

  describe "add" $ do
    it "should add 2 cornflakes" $ do
      add 2 cornflakes empty `shouldBe` M.fromList [(cornflakes, 2)]

    it "should combine the same product" $ do
      let newCart = add 3 cornflakes initialCart
      newCart `shouldBe` M.fromList [(cornflakes, 5)]

  describe "subTotal" $ do
    it "should correctly calculate cornflakes" $ do
      subTotal initialCart `shouldBe` 5.04

  describe "roundCents" $ do
    it "should correctly round up" $ do
      roundCents 0.126 `shouldBe` 0.13

    it "should correctly round down" $ do
      roundCents 0.124 `shouldBe` 0.12

    it "should correctly round .5 up" $ do
      roundCents 0.125 `shouldBe` 0.13

  describe "tax" $ do
    it "should correctly tax cornflakes" $ do
      tax 0.125 initialCart `shouldBe` 0.63

    it "should correctly calculate no tax" $ do
      tax 0.0 initialCart `shouldBe` 0.0

    it "should correctly round the tax" $ do
      let shreddies = ("Shreddies", 1.00)
      let cart = add 3 shreddies empty
      tax 0.125 cart `shouldBe` 0.38

  describe "total" $ do
    it "should correctly calculate total with no tax" $ do
      total 0.0 initialCart `shouldBe` 5.04

    it "should correctly calculate total with tax" $ do
      total 0.125 initialCart `shouldBe` 5.67

    it "should correctly calculate carts with multiple items" $ do
      let doveSoap = ("Dove Soap", 10.0)
      let axeDeo = ("Axe Deo", 5.0)
      let cart = add 2 doveSoap $
                 add 3 doveSoap $
                 add 1 axeDeo $
                 add 2 axeDeo $ empty
      total 0.1 cart `shouldBe` 71.5

  describe "Part 1" $ do
    it "should match the expected input" $ do
      let cart = add 2 cornflakes empty
      let rate = 0.125
      subTotal cart `shouldBe` 5.04
      tax rate cart `shouldBe` 0.63
      total rate cart `shouldBe` 5.67