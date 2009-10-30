{- | A synchronous implementation of the 'Control.Observer.Subject'
     typeclass, , based on Observable.hs by Bastiaan Heeren,
     originally from
     <http://www.cs.uu.nl/wiki/bin/view/Afp0607/ExerciseWXHaskell>

The 'Control.Observer.Subject' implementation defined in this module
uses 'MVar's to provide a simple and threadsafe synchronous
implementation of the Observer design pattern.

Note that no constructor for 'Sub' is exported: client code must use
the 'createSub' smart constructor.

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Observer.Synchronous (
    Sub,
    createSub
) where

import Control.Concurrent.MVar

import Control.Observer

-- | Threadsafe synchronous Subject implementation.

-- A Sub contains:
--   1) an MVar pointer to the current value;
--   2) an MVar pointer to a list of "notify" functions (the
--   observers).
data Sub a = SubC {
    value     :: MVar a,
    observers :: MVar [a -> IO ()]
   }

instance Subject (Sub a) a where 
    -- Read current value; would block if it was ever empty, but it
    -- shouldn't ever be.
    getValue = readMVar . value
    -- Write current value; we can't just use putMVar because it
    -- blocks if the MVar is full.  We want to always overwrite.
    setValue' sub = modifyMVar_ (value sub) . const . return
    -- Append an observer to the list of observers.
    addObserver sub = modifyMVar_ (observers sub) . \x -> return . (++ [x])
    -- Get the current list of observers.
    getObservers = readMVar . observers

-- | Smart constructor for Sub.
createSub :: a -> IO (Sub a)
createSub val = do value'   <- newMVar val
                   observers' <- newMVar []
                   return $ SubC value' observers'
