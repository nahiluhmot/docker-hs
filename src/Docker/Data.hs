-- | This module holds basic data types for the library.
module Docker.Data ( DockerError(..)
                   , DockerState(..)
                   ) where

import qualified Data.ByteString as B
import Network.Http.Client

-- | This data type is used to represent errors throughout the library.
data DockerError = InternalError B.ByteString
                 | UnexpectedHTTPStatus Int
                 | ParseError B.ByteString
                 deriving (Eq, Show)

-- | This data type is used to hold the state of Docker computations.
data DockerState =
    DockerState { auth       :: Maybe ( B.ByteString, B.ByteString
                                      , B.ByteString, B.ByteString)
                , connection :: Connection
                } deriving (Show)
