{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Control.Monad.Indexed.Free.Class (IxMonadFree(..), iliftFree) where
import Control.Monad.Indexed

class IxMonad m => IxMonadFree f m | m -> f where
    iwrap :: f i j (m j k a) -> m i k a

iliftFree :: (IxFunctor f, IxMonadFree f m) => f i j a -> m i j a
iliftFree = iwrap . imap ireturn 