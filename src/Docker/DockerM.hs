{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module holds the 'DockerM' data type, which is a generec interface
-- for computations that can have errors, logging messages, internal state, make
-- requests, and lift other actions.
module Docker.DockerM ( -- * Data Type
                        DockerM(..)
                      , runDockerM
                        -- * Monadic Functions
                      , requestMiddleware
                      , responseMiddleware
                      , log
                      , request
                        -- * Re-exports
                      , module Export
                      ) where

import Control.Monad.Except as Export
import Control.Monad.State.Class as Export
import Control.Monad.Writer.Class as Export
import Control.Applicative
import Data.Monoid
import Prelude hiding (log)

-- | 'DockerM' is a monadic DSL for representing Docker computations.
--
-- Type variables:
--
--  * @e@  - Error type
--
--  * @w@  - Logged type
--
--  * @s@  - State type
--
--  * @r@  - Request type
--
--  * @r'@ - Response type
--
--  * @m@  - Inner 'Monad'
--
--  * @a@  - Return type
data DockerM e w s r r' m a = Pure a
                            | Error e
                            | Log w (DockerM e w s r r' m a)
                            | Put s (DockerM e w s r r' m a)
                            | Get (s -> DockerM e w s r r' m a)
                            | Request r (r' -> DockerM e w s r r' m a)
                            | Lift (m (DockerM e w s r r' m a))

-- | Given a 'DockerM', a logging callback, request/response callback, and
-- initial state, transform the 'DockerM' computation into a computation of
-- its inner monad.
runDockerM :: Monad m
           => DockerM e w s r r' m a -- ^ Computation to run
           -> (w -> m ())            -- ^ Logging callback
           -> (r -> m r')            -- ^ Request/response callback
           -> s                      -- ^ Initial state
           -> m (Either e a, s)      -- ^ Monadic either error or result
runDockerM act logger req i =
    let go s (Pure a)      = return (Right a, s)
        go s (Error e)     = return (Left e, s)
        go _ (Put s n)     = go s n
        go s (Get f)       = go s (f s)
        go s (Request r f) = req r >>= go s . f
        go s (Log w n)     = logger w >> go s n
        go s (Lift action) = action >>= go s
    in  go i act

-- | Add a request middleware of type @x@ by passing a transformation from @x@
-- to @r@.
requestMiddleware :: Monad m
                  => (x -> DockerM e w s r r' m r)
                  -> DockerM e w s x r' m a
                  -> DockerM e w s r r' m a
requestMiddleware f =
        let go (Pure a)      = Pure a
            go (Error e)     = Error e
            go (Put s n)     = Put s (go n)
            go (Get g)       = Get (go . g)
            go (Request x g) = f x >>= flip Request (go . g)
            go (Log w n)     = Log w (go n)
            go (Lift action) = Lift $ liftM go action
        in  go

-- | Add a response middleware of type @x@ by passing a transformation from @r'@
-- to @x@.
responseMiddleware :: Monad m
                   => (r' -> DockerM e w s r r' m x)
                   -> DockerM e w s r x m a
                   -> DockerM e w s r r' m a
responseMiddleware f =
        let go (Pure a)      = Pure a
            go (Error e)     = Error e
            go (Put s n)     = Put s (go n)
            go (Get g)       = Get (go . g)
            go (Request r g) = Request r (go . g <=< f)
            go (Log w n)     = Log w (go n)
            go (Lift action) = Lift $ liftM go action
        in  go


-- | Log a value.
log :: w -> DockerM e w s r r' m ()
log = flip Log (Pure ())

-- | Send a request, wrappping the response in a 'DockerM'.
request :: r -> DockerM e w s r r' m r'
request r = Request r Pure

instance (Monad m, Monoid e) => Monoid (DockerM e w s r r' m a) where
    mempty = Error mempty
    mappend a b = a `catchError` const b

instance Monad m => Functor (DockerM e w s r r' m) where
    fmap f =
        let go (Pure a)      = Pure $ f a
            go (Error e)     = Error e
            go (Put s n)     = Put s (go n)
            go (Get g)       = Get (go . g)
            go (Request r g) = Request r (go . g)
            go (Log w n)     = Log w (go n)
            go (Lift action) = Lift $ liftM go action
        in  go

instance Monad m => Applicative (DockerM e w s r r' m) where
    pure = Pure
    (<*>) = ap

instance Monad m => Monad (DockerM e w s r r' m) where
    return = Pure
    d >>= f =
        let go (Pure a)      = f a
            go (Error e)     = Error e
            go (Put s n)     = Put s (go n)
            go (Get g)       = Get (go . g)
            go (Request r g) = Request r (go . g)
            go (Log w n)     = Log w (go n)
            go (Lift action) = Lift $ liftM go action
        in  go d

instance (Monad m, Monoid e) => MonadPlus (DockerM e w s r r' m) where
    mzero = mempty
    mplus = mappend

instance (Monad m, Monoid e) => Alternative (DockerM e w s r r' m) where
    empty = mempty
    (<|>) = mappend

instance Monad m => MonadError e (DockerM e w s r r' m) where
    throwError = Error
    catchError d h =
        let go (Pure a)      = Pure a
            go (Error e)     = h e
            go (Put s n)     = Put s (go n)
            go (Get g)       = Get (go . g)
            go (Request r g) = Request r (go . g)
            go (Log w n)     = Log w (go n)
            go (Lift action) = Lift $ liftM go action
        in  go d

instance Monad m => MonadState s (DockerM e w s r r' m) where
    get = Get Pure
    put = flip Put (Pure ())

instance (Monad m, Monoid w) => MonadWriter w (DockerM e w s r r' m) where
    writer (a, w) = Log w (Pure a)
    tell w = Log w (Pure ())
    listen =
        let go acc (Pure a)      = return (a, acc)
            go _   (Error e)     = Error e
            go acc (Put s n)     = Put s (go acc n)
            go acc (Get g)       = Get (go acc . g)
            go acc (Request r g) = Request r (go acc . g)
            go acc (Log w n)     = Log w $ go (acc `mappend` w) n
            go acc (Lift action) = Lift $ liftM (go acc) action
        in  go mempty
    pass m = listen m >>= \((a, f), w) -> Log (f w) (Pure a)

instance MonadIO m => MonadIO (DockerM e w s r r' m) where
    liftIO = lift . liftIO

instance MonadTrans (DockerM e w s r r') where
    lift = Lift . liftM Pure
