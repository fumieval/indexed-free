{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, Rank2Types, PolyKinds, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Indexed.Free
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Monad.Indexed.Free (IxFree(..), hoistIxFree, module Control.Monad.Indexed.Free.Class) where

import Control.Applicative
import Control.Monad.Indexed
import Control.Monad.Indexed.Free.Class

data IxFree f i j x where
    Pure :: a -> IxFree f i i a
    Free :: f i j (IxFree f j k a) -> IxFree f i k a

instance IxFunctor f => IxFunctor (IxFree f) where
    imap f (Pure a) = Pure (f a)
    imap f (Free w) = Free (imap (imap f) w)

instance IxFunctor f => IxPointed (IxFree f) where
    ireturn = Pure

instance IxFunctor f => IxApplicative (IxFree f) where
    iap (Pure a) (Pure b) = Pure (a b)
    iap (Pure a) (Free fb) = Free (imap a `imap` fb)
    iap (Free fa) mb = Free $ imap (`iap` mb) fa

instance IxFunctor f => IxMonad (IxFree f) where
    ibind k (Pure a) = k a
    ibind k (Free fm) = Free $ imap (ibind k) fm

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

hoistIxFree :: (IxFunctor g, IxMonadFree g m) => (forall i j x. f i j x -> g i j x) -> IxFree f i j a -> m i j a
hoistIxFree _ (Pure a) = ireturn a
hoistIxFree f (Free fm) = iwrap $ imap (hoistIxFree f) $ f fm