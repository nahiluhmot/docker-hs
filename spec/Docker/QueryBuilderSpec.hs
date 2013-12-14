{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Docker.QueryBuilderSpec ( spec ) where

import Docker.QueryBuilder
import Test.Hspec

spec :: Spec
spec = do
    describe "compileQuery" $ do
        context "when there are not parameters" $ do
            let qStr = compileQuery noParams

            it "is empty" $ do
                qStr `shouldBe` ""

        context "when there is one parameter" $ do
            context "param" $ do
                let qStr = compileQuery $ param "number" (15 :: Int)
                it "and it does not need to be escaped" $ do
                    qStr `shouldBe` "?number=15"

            context "and it is needs to be escaped" $ do
                let qStr = compileQuery $ stringParam "val" ">something"

                it "adds the parameter" $ do
                    qStr `shouldBe` "?val=%3Esomething"

        context "when there are many parameters" $ do
            let qStr = compileQuery $ do
                    param "int" (10 :: Int)
                    stringParam "string" ">lol<"
                    param "double" (2.124 :: Double)

            it "adds every parameter" $ do
                qStr `shouldBe` "?int=10&string=%3Elol%3C&double=2.124"
