{-# LANGUAGE GADTs #-}
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

import Control.Monad.Indexed

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
