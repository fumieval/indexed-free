{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, Rank2Types, PolyKinds, UndecidableInstances #-}
module Control.Monad.Indexed.Trans.Free (IxFreeF(..), IxFreeT(..), transIxFreeT, module Control.Monad.Indexed.Free.Class) where

import Control.Applicative
import Control.Monad.Indexed
import Control.Monad.Indexed.Free.Class

data IxFreeF f m i j a where
    Pure :: a -> IxFreeF f m i i a
    Free :: f i j (IxFreeT f m j k a) -> IxFreeF f m i k a

newtype IxFreeT f m i j a = IxFreeT { runIxFreeT :: m (IxFreeF f m i j a) }

bindIxFreeT :: (IxFunctor f, Monad m) => IxFreeT f m i j a -> (a -> IxFreeT f m j k b) -> IxFreeT f m i k b
bindIxFreeT (IxFreeT m) k = IxFreeT $ m >>= \r -> case r of
    Pure a -> runIxFreeT (k a)
    Free f -> return $ Free $ imap (`bindIxFreeT` k) f

instance (Monad m, IxFunctor f) => IxFunctor (IxFreeT f m) where
    imap f (IxFreeT m) = IxFreeT $ m >>= \r -> case r of
        Pure a -> return $ Pure (f a)
        Free fm -> return $ Free $ imap (imap f) fm

instance (Monad m, IxFunctor f) => IxPointed (IxFreeT f m) where
    ireturn = IxFreeT . return . Pure

instance (Monad m, IxFunctor f) => IxApplicative (IxFreeT f m) where
    iap mf m = bindIxFreeT mf $ \f -> bindIxFreeT m $ \a -> return (f a)

instance (Monad m, IxFunctor f) => IxMonad (IxFreeT f m) where
    ibind = flip bindIxFreeT

instance (Monad m, IxFunctor f) => IxMonadFree f (IxFreeT f m) where
    iwrap = IxFreeT . return . Free

instance (Monad m, IxFunctor f) => Functor (IxFreeT f m i i) where
    fmap = imap

instance (Monad m, IxFunctor f) => Applicative (IxFreeT f m i i) where
    pure = ireturn
    (<*>) = iap

instance (Monad m, IxFunctor f) => Monad (IxFreeT f m i i) where
    return = ireturn
    (>>=) = (>>>=)

transIxFreeT :: (IxFunctor g, Monad m) => (forall i j x. f i j x -> g i j x) -> IxFreeT f m i j a -> IxFreeT g m i j a
transIxFreeT f (IxFreeT m) = IxFreeT $ m >>= \r -> return $ case r of
    Pure a -> Pure a
    Free fm -> Free $ imap (transIxFreeT f) (f fm)

hoistIxFreeT :: (IxFunctor f, Monad m) => (forall x. m x -> n x) -> IxFreeT f m i j a -> IxFreeT f n i j a
hoistIxFreeT t (IxFreeT m) = IxFreeT $ t $ m >>= \r -> return $ case r of
    Pure a -> Pure a
    Free fm -> Free $ imap (hoistIxFreeT t) fm