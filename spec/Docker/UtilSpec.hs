{-# LANGUAGE OverloadedStrings #-}

module Docker.UtilSpec ( spec ) where

import Control.Monad.RWS
import qualified Data.Aeson as A
import Docker.Base
import Docker.Util
import Test.Hspec
import qualified Data.ByteString.Char8 as BS
import qualified Network.Http.Client as H
import System.IO.Error
import System.IO.Streams

spec :: Spec
spec = do
    describe "charToWord" $ do
        it "converts the character to its ASCII value" $ do
            charToWord 'a' `shouldBe` 97

    describe "wordToChar" $ do
        it "converts the ASCII value to a character" $ do
            wordToChar 48 `shouldBe` '0'

    describe "toLazy" $ do
        it "converts a Char8 ByteString to a Lazy ByteString" $ do
            toLazy "test string" `shouldBe` "test string"

    describe "readAll" $ do
        it "reads every piece of the Stream" $ do
            cabalFile <- BS.readFile "docker-hs.cabal"
            stream <- fromByteString cabalFile
            cabalFile' <- readAll stream
            cabalFile `shouldBe` cabalFile'

    describe "simpleRequest" $ do
        it "receives the entire response" $ do
            errorOrResult <- tryIOError $ do
                (res, _) <- execDocker "127.0.0.1" 9902 $ do
                    req <- liftIO . H.buildRequest $ do
                        H.http H.GET "/info"
                        H.setAccept "application/json"
                    resp <- simpleRequest req H.emptyBody
                    case A.decode $ toLazy resp of
                        Just value -> return value
                        _ -> (parseError $ resp) :: Docker A.Value

                case res of
                    Right _ -> True `shouldBe` True
                    _ -> False `shouldBe` True
            
            case errorOrResult of
                Right _ -> return ()
                Left _ -> do
                    putStrLn "This test requires Docker to run on port 9902."
                    False `shouldBe` True
