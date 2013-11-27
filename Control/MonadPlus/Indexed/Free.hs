{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses #-}
module Control.MonadPlus.Indexed.Free (IxFree(..), module Control.Monad.Indexed.Free.Class) where

import Control.Applicative
import Control.Monad.Indexed
import Control.Monad.Indexed.Free.Class

data IxFree f i j x where
    Pure :: a -> IxFree f i i a
    Free :: f i j (IxFree f j k a) -> IxFree f i k a
    Plus :: [IxFree f i j a] -> IxFree f i j a

instance IxFunctor f => IxFunctor (IxFree f) where
    imap f (Pure a) = Pure (f a)
    imap f (Free w) = Free (imap (imap f) w)
    imap f (Plus s) = Plus (map (imap f) s)

instance IxFunctor f => IxPointed (IxFree f) where
    ireturn = Pure

instance IxFunctor f => IxApplicative (IxFree f) where
    iap (Pure a) (Pure b) = Pure (a b)
    iap (Pure a) (Free fb) = Free (imap a `imap` fb)
    iap (Free fa) mb = Free $ imap (`iap` mb) fa

instance IxFunctor f => IxMonadZero (IxFree f) where
    imzero = Plus []

instance IxFunctor f => IxMonad (IxFree f) where
    ibind k (Pure a) = k a
    ibind k (Free fm) = Free $ imap (ibind k) fm
    ibind k (Plus m) = Plus (map (ibind k) m)

instance IxFunctor f => IxMonadPlus (IxFree f) where
  Plus [] `implus` r = r
  l `implus` Plus [] = l
  Plus as `implus` Plus bs = Plus (as ++ bs)
  a `implus` b = Plus [a, b]

instance IxFunctor f => IxMonadFree f (IxFree f) where
    iwrap = Free

instance IxFunctor f => Functor (IxFree f i i) where
    fmap = imap

instance IxFunctor f => Applicative (IxFree f i i) where
    pure = ireturn
    (<*>) = iap

instance IxFunctor f => Monad (IxFree f i i) where
    return = ireturn
    (>>=) = (>>>=)

