{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Indexed.Free
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Monad.Indexed.Free (IxFree(..)) where

import Control.Applicative
import Control.Monad.Indexed
import Control.Monad.Indexed.Free.Class

data IxFree f i j x where
    IxPure :: a -> IxFree f i i a
    IxFree :: f i j (IxFree f j k a) -> IxFree f i k a

instance IxFunctor f => IxFunctor (IxFree f) where
    imap f (IxPure a) = IxPure (f a)
    imap f (IxFree w) = IxFree (imap (imap f) w)

instance IxFunctor f => IxPointed (IxFree f) where
    ireturn = IxPure

instance IxFunctor f => IxApplicative (IxFree f) where
    iap (IxPure a) (IxPure b) = IxPure (a b)
    iap (IxPure a) (IxFree fb) = IxFree (imap a `imap` fb)
    iap (IxFree fa) mb = IxFree $ imap (`iap` mb) fa

instance IxFunctor f => IxMonad (IxFree f) where
    ibind k (IxPure a) = k a
    ibind k (IxFree fm) = IxFree $ imap (ibind k) fm

instance IxFunctor f => IxMonadFree f (IxFree f) where
    iwrap = IxFree

instance IxFunctor f => Functor (IxFree f i i) where
    fmap = imap

instance IxFunctor f => Applicative (IxFree f i i) where
    pure = ireturn
    (<*>) = iap

instance IxFunctor f => Monad (IxFree f i i) where
    return = ireturn
    (>>=) = (>>>=)
