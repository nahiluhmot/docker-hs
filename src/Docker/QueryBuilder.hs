{-# LANGUAGE OverloadedStrings #-}

-- | This module holds all of the logic used to build queries.
module Docker.QueryBuilder ( QueryBuilder
                           , isAllowedQS
                           , param
                           , stringParam
                           , noParams
                           , compileQuery
                           ) where

import Control.Monad.Writer.Strict
import qualified Data.ByteString.Char8 as BS
import Data.Char (isAlphaNum)
import Data.List (intersperse)
import Network.URI (escapeURIString)

-- | This type is used to build query parameters.
type QueryBuilder a = Writer [BS.ByteString] a

-- | Add a query parameter.
param :: Show a => BS.ByteString -> a -> QueryBuilder ()
param key = stringParam key . BS.pack . show

-- | Test if the given 'Char' is allowed in the query.
isAllowedQS :: Char -> Bool
isAllowedQS char = elem char "-_.~" || isAlphaNum char

-- | Add a 'BS.ByteString' query parameter.
stringParam :: BS.ByteString -> BS.ByteString -> QueryBuilder ()
stringParam key val = tell . return $ key' `BS.append` ('=' `BS.cons` val')
    where key' = escp key
          val' = escp val
          escp = BS.pack . escapeURIString isAllowedQS . BS.unpack

-- | Convenience method to show that there are no parameters.
noParams :: QueryBuilder ()
noParams = return ()

-- | Turn the 'QueryBuilder' into a 'BS.ByteString'.
compileQuery :: QueryBuilder a -> BS.ByteString
compileQuery query =
    case execWriter query of
        [] -> ""
        qs -> '?' `BS.cons` mconcat (intersperse "&" qs)
