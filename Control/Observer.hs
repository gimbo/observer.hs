-- Observer pattern, based on Observable.hs by Bastiaan Heeren,
-- http://www.cs.uu.nl/wiki/bin/view/Afp0607/ExerciseWXHaskell

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Observer (
    Subject(..),
    setValue,
    notifyObservers,
    changeValue,
    addConstObserver
) where

-- | A type class for observable objects.
class Subject sub val | sub -> val where
    -- | Get the subject's current value.
    getValue     :: sub -> IO val
    -- | Update the subject's value quietly; should NOT call
    -- notifyObservers.
    setValue'    :: sub -> val -> IO ()
    -- | Add an observer.
    addObserver  :: sub -> (val -> IO ()) -> IO ()
    -- | Get the list of observers.
    getObservers :: sub -> IO [val -> IO ()]

-- | Update the subject value, and notify observers.
setValue :: Subject sub val => sub -> val -> IO ()
setValue subject value = do subject `setValue'` value
                            notifyObservers subject

-- | Notify observers that the subject's value has changed.
notifyObservers :: Subject sub val => sub -> IO ()
notifyObservers subject =
   do value <- getValue subject
      observers <- getObservers subject
      mapM_ ($ value) observers

-- | Apply an update function to the subject value, and notify
-- observers.
changeValue :: Subject sub val => sub -> (val -> val) -> IO ()
changeValue subject f =
   do a <- getValue subject
      subject `setValue` f a

-- | Add an observer which doesn't care about the subject's value,
-- only that it's changed.
addConstObserver  :: Subject sub val => sub -> IO () -> IO ()
addConstObserver sub = addObserver sub . const
