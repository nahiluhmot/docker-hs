{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- | This module holds all of the utilities used throughout the rest of the
-- library.
module Docker.Util ( charToWord
                   , wordToChar
                   , toLazy
                   , readAll
                   , simpleRequest
                   ) where

import Control.Monad.RWS
import qualified Blaze.ByteString.Builder as B
import qualified Data.Word as W
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LS
import Docker.Base
import qualified System.IO.Streams as S
import qualified Network.Http.Client as H

-- | Convert a 'Char' to a 'Word8'.
charToWord :: Char -> W.Word8
charToWord = fromIntegral . fromEnum

-- | Convert a 'Word8' to a 'Char'.
wordToChar :: W.Word8 -> Char
wordToChar = toEnum . fromIntegral

-- | Convert a 'BS.ByteString' to a 'LS.ByteString'.
toLazy :: BS.ByteString -> LS.ByteString
toLazy = LS.pack . map charToWord . BS.unpack

-- | Read every value from the given S.inputStream.
readAll :: Monoid a => S.InputStream a -> IO a
readAll stream =
    let go acc = do
            val <- S.read stream
            case val of
                Just curr -> go $ acc `mappend` curr
                _         -> return acc
    in  go mempty

-- | Send a simple HTTP request.
simpleRequest :: H.Request ->
                 (S.OutputStream B.Builder -> IO a) ->
                 Docker BS.ByteString
simpleRequest request body = do
    conn <- getConnection
    liftIO $ H.sendRequest conn request body

    (code, str) <- liftIO . H.receiveResponse conn $ \resp stream -> do
        value <- readAll stream
        return (H.getStatusCode resp, value)

    when (code >= 500) $ serverError str
    when (code >= 400) $ clientError str

    return str
