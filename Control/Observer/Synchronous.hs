{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Observer.Synchronous (
    Sub,
    createSub
) where

import Data.IORef

import Control.Observer

-- | Synchronous Subject implementation.
--
-- An Sub contains:
--   1) a pointer to the current value;
--   2) a pointer to a list of "notify" functions (of the observers).
data Sub a = SubC {
    subject   :: IORef a,
    observers :: IORef [a -> IO ()]
   }

instance Subject (Sub a) a where 
    getValue = readIORef . subject
    setValue'= writeIORef . subject
    addObserver observable = modifyIORef (observers observable) . (:)
    getObservers = readIORef . observers

-- | Smart constructor for Sub.
createSub :: a -> IO (Sub a)
createSub value = do subject'   <- newIORef value
                     observers' <- newIORef []
                     return $ SubC subject' observers'
