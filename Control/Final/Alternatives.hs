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
-- Module: Control.Final.Alternatives
-- License: BSD3
--
-- An alternative to "Control.Final" is a using the writer monad.
--
-- > instance Monoid a => Monoid (IO a) where
-- >   mempty = return mempty
-- >   mappend = liftA2 mappend
-- >
-- > stmExampleW = do
-- >   tv <- newTVarIO "xxx"
-- >   join $ atomically $ execWriterT $ do
-- >     val <- lift $ readTVar tv
-- >     case val of
-- >       "xxx" -> do
-- >         lift $ writeTVar tv "foobar"
-- >         tell $ print "it was xxx"
-- >       "yyy" -> do
-- >         lift $ writeTVar tv "quux"
-- >         tell $ print "it was yyy"
-- >     tell $ print "I finished"
--
-- This works.  Unfortunately when using the writer, you have to 'lift'
-- every operation of the base monad and also there is no such thing
-- as @finalTell@ to specify that this is a return point of the
-- function.  On the positive side, the writer alternative let's you
-- gather multiple 'IO' operations together using a 'Monoid', while
-- 'final' can't do that.

module Control.Final.Alternatives where

-- These imports are only here for haddock.
import Control.Final
import Control.Monad.Trans.Class
import Data.Monoid
