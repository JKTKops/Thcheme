module P where

import Control.Monad (liftM, ap)

newtype Writer m a = Writer { runWriter :: (m, a) }   

instance (Monoid m) => Monad (Writer m) where
    return x = Writer (mempty, x)
    m >>= f  = let (log, x) = runWriter m
                   (log', x') = runWriter $ f x
                in Writer (log `mappend` log', x')

instance (Monoid m) => Applicative (Writer m) where
    pure  = return
    (<*>) = ap

instance Functor (Writer m) where
    fmap f w = let (log, x) = runWriter w
                in Writer (log, f x)

tell :: (Monoid m) => m -> a -> Writer m a
tell message val = Writer (message, val) 
