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

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module: Control.Final.Example
-- License: BSD3
--
-- Consider this peace of code:
--
-- > join $ atomically $ do
-- >   radarPositive <- readTVar radarTVar
-- >   launchKeyInserted <- readTVar launchKeyTVar
-- >   case radarPositive of
-- >     False -> do
-- >       modifyTVar radarNegativeCounter (+1)
-- >       return $ print "No need for missiles, it's peaceful"
-- >     True -> do
-- >       modifyTVar radarPositiveCounter (+1)
-- >       case launchKeyInserted of
-- >         False -> do
-- >           modifyTVar keyMissingCounter (+1)
-- >           return $ print "No launch key, ignoring radar"
-- >         True -> do
-- >           modifyTVar launchCounter (+1)
-- >           return $ launchMissiles
-- >   return $ print "extra debug: state checking finished"
--
-- We use 'STM' to make state checking of multiple 'TVar's one atomic
-- transaction.  Since we can't do 'IO' in 'STM', we are just returning
-- the 'IO' that needs to be done in the different cases.  Unfortunately
-- when we try to add the extra debugging as the last statement, that
-- silently ignores all the previous \"return\" values, even
-- @launchMissiles@.
--
-- On the other hand when using final:
--
-- > atomicJoinFinal $ do
-- >   radarPositive <- readTVar radarTVar
-- >   launchKeyInserted <- readTVar launchKeyTVar
-- >   case radarPositive of
-- >     False -> do
-- >       modifyTVar radarNegativeCounter (+1)
-- >       final $ print "No need for missiles, it's peaceful"
-- >     True -> do
-- >       modifyTVar radarPositiveCounter (+1)
-- >       case launchKeyInserted of
-- >         False -> do
-- >           modifyTVar keyMissingCounter (+1)
-- >           final $ print "No launch key, ignoring radar"
-- >         True -> do
-- >           modifyTVar launchCounter (+1)
-- >           final $ launchMissiles
-- >   final $ print "extra debug: state checking finished"
--
-- We get a compile error that contains this:
--
-- >    Note: there are several potential instances:
-- >      instance FinalClass Control.Final.FinalTooManyReturns
--
-- Internally @Final@ is based on ambiguity checking in the type
-- system.  The prohibited ambiguity occurs, because the only way to
-- decide what 'final' means is by matching it to the corresponding
-- 'atomicJoinFinal'.  This is now only possible for the 'final' at
-- the end of the function and not for the middle ones, so we get the
-- error.

module Control.Final.Example where

-- These imports are only here for haddock.
import Control.Concurrent.STM
import Control.Final
import Control.Monad.Trans.Class
