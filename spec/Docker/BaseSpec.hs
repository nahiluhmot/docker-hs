{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Docker.BaseSpec ( spec ) where

import Control.Monad.Error
import Network.Http.Client
import Docker.Base
import Prelude hiding (log)
import System.IO.Streams
import Test.Hspec

spec :: Spec
spec = do
    let makeFake = do
            i <- nullInput
            o <- nullOutput
            makeConnection "localhost" (return ()) o i
            
    describe "execDocker'" $ do
        context "when the computation fails" $ do
            it "returns the error and logs" $ do
                conn <- makeFake
                (res, logs) <- execDocker' conn $ do
                    log "first"
                    log "second"
                    throwError noMsg
                    log "third"
                res `shouldBe` Left (BaseError "")
                logs `shouldBe` ["first", "second"]

        context "when the computation passes" $ do
            it "returns the result and logs" $ do
                conn <- makeFake
                (res, logs) <- execDocker' conn $ do
                    log "first"
                    log "second"
                    log "third"
                    return 'a'
                res `shouldBe` Right 'a'
                logs `shouldBe` ["first", "second", "third"]

    describe "MonadReader Connection Docker" $ do
        it "returns the connection" $ do
            conn <- makeFake
            (res, _) <- execDocker' conn (getConnection >>= return . show)
            res `shouldBe` Right "Host: localhost\n"

    describe "MonadState (Maybe Auth) Docker" $ do
        context "when no login has occurred" $ do
            it "returns 'Nothing'" $ do
                conn <- makeFake
                (res, _) <- execDocker' conn getAuth
                res `shouldBe` Right Nothing

        context "when a login has occurred" $ do
            let auth = Auth { username = "nahiluhmot"
                            , password = "i<3docker"
                            , email    = "hulihan.tom159@gmail.com"
                            , server   = "http://localhost"
                            }

            it "returns 'Just auth'" $ do
                conn <- makeFake
                (res, _) <- execDocker' conn $ login auth >> getAuth
                res `shouldBe` Right (Just auth)

            context "and then a logout occurs" $
                it "returns 'Nothing'" $ do
                    conn <- makeFake
                    (res, _) <- execDocker' conn $ do
                        login auth
                        logout
                        getAuth
                    res `shouldBe` Right Nothing

    describe "MonadWriter [BS.ByteString] Docker" $ do
        it "writes each log message" $ do
            conn <- makeFake
            (_, logs) <- execDocker' conn $ do
                log "yolo"
                log "swag"
            logs `shouldBe` ["yolo", "swag"]

    describe "MonadError DockerError Docker" $ do
        context "when there is a clientError" $ do
            it "throws a ClientError" $ do
                conn <- makeFake
                (res, _) <- execDocker' conn (clientError "woops" :: Docker ())
                res `shouldBe` Left (ClientError "woops")

        context "when theere is a serverError" $ do
            it "throws a ServerError" $ do
                conn <- makeFake
                (res, _) <- execDocker' conn (serverError "weeps" :: Docker ())
                res `shouldBe` Left (ServerError "weeps")

        context "when theere is a parseError" $ do
            it "throws a ParseError" $ do
                conn <- makeFake
                (res, _) <- execDocker' conn (parseError "weeps" :: Docker ())
                res `shouldBe` Left (ParseError "weeps")
