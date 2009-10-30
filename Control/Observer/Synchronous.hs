{- | A synchronous implementation of the 'Control.Observer.Subject'
     typeclass, , based on Observable.hs by Bastiaan Heeren,
     originally from
     <http://www.cs.uu.nl/wiki/bin/view/Afp0607/ExerciseWXHaskell>

The 'Control.Observer.Subject' implementation defined in this module
uses 'IORef's to provide a simple synchronous implementation of the
Observer design pattern.

Note that no constructor for 'Sub' is exported: client code must use
the 'createSub' smart constructor.

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Observer.Synchronous (
    Sub,
    createSub
) where

import Data.IORef

import Control.Observer

-- | Synchronous Subject implementation.

-- A Sub contains:
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
