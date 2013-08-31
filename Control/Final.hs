-- Copyright (c) 2013, Gergely Risko  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
--    Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
--    Redistributions in binary form must reproduce the above
--    copyright notice, this list of conditions and the following
--    disclaimer in the documentation and/or other materials provided
--    with the distribution.
--
--    Neither the name of the author nor the names of his contributors
--    may be used to endorse or promote products derived from this
--    software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
-- FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
-- COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
-- INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
-- OF THE POSSIBILITY OF SUCH DAMAGE.
--
-- Authors:
--   Gergely Risko <gergely@risko.hu>
--   Mihaly Barasz <mihaly@barasz.com>

-- |
-- Module: Control.Final
-- License: BSD3
--
-- The @Final@ library makes it possible to point out return values in
-- (monadic) functions.  This adds extra compile-time safety to your
-- code, because the compiler will warn you if marked return values
-- accidentally became middle parts of a big function written with do
-- notation.
--
-- A worked out example can be found in "Control.Final.Example", and
-- an alternative approach is considered in "Control.Final.Alternatives".

module Control.Final (
  -- * Most common, monadic usage
  final,
  runFinal,
  joinFinal,
  atomicJoinFinal,
  -- * Related functions
  atomic,
  -- * Usage in pure code
  FinalClass(finalV),
  runFinalV,
  ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class

-- | This class is the main idea behind @Final@.  @finalV@ is in a
-- type class, so we we can have to instances, therefore if used
-- incorrectly the compiler sees an ambiguity.
class FinalClass f where
  finalV :: a -> f a

newtype Final a = Final {
  -- | Usage example:
  --
  -- > pureExample p =
  -- >   runFinalV $ case p of
  -- >     True -> finalV "quux"
  -- >     False -> finalV "foobar"
  runFinalV :: a
  }
newtype FinalTooManyReturns a = FinalTooManyReturns a

instance FinalClass Final where
  {-# INLINE finalV #-}
  finalV = Final

instance FinalClass FinalTooManyReturns where
  finalV = FinalTooManyReturns

-- | Use this instead of 'return' when you want to mark this point as
-- a final return for the corresponding 'runFinal'.
final :: (Monad m, FinalClass f) => x -> m (f x)
{-# INLINE final #-}
final = return . finalV

-- | Most commonly used at the beginning of functions to mark the
-- point in the code where 'final' calls \"return to\".
--
-- This compiles:
--
-- > simple = runFinal $ do
-- >   print "line1"
-- >   do return False
-- >      return True
-- >   final True
--
-- This fails at the first \"@final@ @True@\":
--
-- > simple = runFinal $ do
-- >   print "line1"
-- >   do return False
-- >      final True
-- >   final True
runFinal :: Monad m => m (Final a) -> m a
{-# INLINE runFinal #-}
runFinal = liftM runFinalV

-- | Useful when the final return value is a monadic computation
-- itself that after returned has to be joined into the current
-- computation.
joinFinal :: (Monad m) => m (Final (m a)) -> m a
{-# INLINE joinFinal #-}
joinFinal = join . runFinal

-- | A version of atomically, that joins and runFinals.  This is the
-- most common usage for the author.  Details and example can be found
-- in "Control.Final.Example".
atomicJoinFinal :: (Functor m, MonadIO m) => STM (Final (m a)) -> m a
{-# INLINE atomicJoinFinal #-}
atomicJoinFinal = joinFinal . liftIO . atomically

-- | MonadIO version of atomically.  Not really related to @Final@,
-- but if we already have 'atomicJoinFinal' in this module, it makes
-- sense to provide this too.
atomic :: MonadIO m => STM a -> m a
{-# INLINABLE atomic #-}
atomic = liftIO . atomically
