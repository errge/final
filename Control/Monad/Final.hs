module Control.Monad.Final ( final, runFinal_, runFinal, joinFinal, FinalClass, Final ) where

import Control.Monad

class FinalClass f where
  final_ :: a -> f a

newtype Final a = Final { runFinal_ :: a }
newtype FinalTooManyReturns a = FinalTooManyReturns a

instance FinalClass Final where
  {-# INLINE final_ #-}
  final_ = Final

instance FinalClass FinalTooManyReturns where
  final_ = FinalTooManyReturns

final :: (FinalClass f, Monad m) => a -> m (f a)
{-# INLINE  final #-}
final = return . final_

joinFinal :: (Monad m, Functor m) => m (Final (m a)) -> m a
{-# INLINE joinFinal #-}
joinFinal = join . fmap runFinal_

runFinal :: Monad m => m (Final a) -> m a
runFinal = liftM runFinal_
