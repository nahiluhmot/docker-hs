{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | This module holds the base monads used throughout the library.
module Docker.Base ( Docker
                   , Auth(..)
                   , DockerError(..)
                   , Logs
                   , runDocker
                   , execDocker
                   , execDocker'
                   , getConnection
                   , getAuth
                   , login
                   , logout
                   , log
                   , clientError
                   , serverError
                   , parseError
                   ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.RWS.Strict
import qualified Data.ByteString.Char8 as BS
import qualified Network.Http.Client as H
import Prelude hiding (log)

-- | This data type represents authentication credentials for a certain Docker
-- server.
data Auth = Auth { username :: BS.ByteString
                 , password :: BS.ByteString
                 , email    :: BS.ByteString
                 , server   :: BS.ByteString
                 } deriving (Eq, Show)

-- | The 'Logs' are just a list of 'BS.ByteString's.
type Logs = [BS.ByteString]

-- | A 'DockerError' can stem from one of the following:
data DockerError
    -- | 'ClientError' is thrown when there is a malformed request.
    = ClientError BS.ByteString
    -- | 'ServerError' is thrown when the server cannot complete the requset.
    | ServerError BS.ByteString
    -- | 'ParseError' is thrown when the server cannot complete the requset.
    | ParseError BS.ByteString
    -- | 'BaseError' is never actually thrown in the library, but is used as for
    -- making this datatype an instance of 'Error'.
    | BaseError BS.ByteString
    deriving (Eq, Show)

-- | The 'Docker' type is a transformer stack of 'RWST' and 'ErrorT'. The reader
-- value is a 'H.Connection', the state value is a 'Maybe Auth', the writter value
-- is a list of 'BS.ByteString's, and the error that maybe be thrown is a
-- 'DockerError'. These two monads wrap 'IO' so that HTTP requests and file IO
-- may be performed.
newtype Docker a =
    Docker (ErrorT DockerError (RWST H.Connection Logs (Maybe Auth) IO) a)
    deriving (Functor, Applicative, Monad, MonadIO)

-- Make 'Docker' instances of the required type classes.
instance Error DockerError where
    strMsg = BaseError . BS.pack

instance MonadError DockerError Docker where
    throwError = Docker . throwError
    catchError comp handler =
        Docker $ (runDocker comp) `catchError` (runDocker . handler)

instance MonadReader H.Connection Docker where
    ask = Docker ask
    local f = Docker . local f . runDocker

instance MonadWriter Logs Docker where
    tell = Docker . tell
    listen = Docker . listen . runDocker
    pass = Docker . pass . runDocker

instance MonadState (Maybe Auth) Docker where
    get = Docker get
    put = Docker . put

-- | Transform the 'Docker' computation into the actual 'Monad' stack it holds.
runDocker :: Docker a ->
             ErrorT DockerError (RWST H.Connection Logs (Maybe Auth) IO) a
runDocker (Docker comp) = comp

-- | Given a 'H.Hostname', 'H.Port', and 'Docker' computation, returns a tuple.
-- The first value in the tuple is either the 'DockerError' that was thrown or
-- the result of the 'Docker' computation; the second value is the 'Logs'
-- recorded during the computation.
execDocker :: H.Hostname -> H.Port -> Docker a ->
              IO (Either DockerError a, Logs)
execDocker host port docker = do
    conn <- H.openConnection host port
    result <- execDocker' conn docker
    H.closeConnection conn
    return result

-- | Much like 'execDocker', except that it provides no 'H.Connection'
-- management.
execDocker' :: H.Connection -> Docker a -> IO (Either DockerError a, Logs)
execDocker' conn docker = do
    (x, _, logs) <- runRWST (runErrorT $ runDocker docker) conn Nothing
    return (x, logs)

-- | Retreive the 'H.Connection' with which this computation was initiated.
getConnection :: Docker H.Connection
getConnection = ask

-- | Get the current 'Auth' credentials (if any).
getAuth :: Docker (Maybe Auth)
getAuth = get

-- | Set the current 'Auth' credentials.
login :: Auth -> Docker ()
login = put . Just

-- | Remove the current 'Auth' credentials.
logout :: Docker ()
logout = put Nothing

-- | Log a 'BS.ByteString'.
log :: BS.ByteString -> Docker ()
log = tell . return

-- | Convenience method to raise a 'ClientError'.
clientError :: BS.ByteString -> Docker a
clientError = throwError . ClientError

-- | Convenience method to raise a 'ServerError'.
serverError :: BS.ByteString -> Docker a
serverError = throwError . ServerError

-- | Convenience method to raise a 'ParseError'.
parseError :: BS.ByteString -> Docker a
parseError = throwError . ParseError
