import Control.Applicative
import Control.Monad.Final
import Control.Monad

import Control.Concurrent.STM

simple = runFinal $ do
  print "line1"
  do return False  -- definitely a problem, we should fail
     return True    -- ok only if this is the last statement
  final True  -- this is definitely OK!

stmExample = do
  tv <- newTVarIO "xxx"
  joinFinal $ atomically $ do
    val <- readTVar tv
    case val of
      "xxx" -> do
        writeTVar tv "foobar"
        final $ print "it was xxx"
      "yyy" -> do
        writeTVar tv "quux"
        final $ print "it was yyy"
