import Control.Applicative
import Control.Final
import Control.Monad
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.Class
import Control.Concurrent.STM
import Data.Monoid

simple = runFinal $ do
  print "line1"
  do return False  -- definitely a problem, we should fail
     return True   -- ok only if this is the last statement
  final True  -- this is definitely OK!

stmExample = do
  tv <- newTVarIO "xxx"
  atomicJoinFinal $ do
    val <- readTVar tv
    case val of
      "xxx" -> do
        writeTVar tv "foobar"
        final $ print "it was xxx"
      "yyy" -> do
        writeTVar tv "quux"
        final $ print "it was yyy"
    -- return $ print "end"

instance Monoid a => Monoid (IO a) where
  mempty = return mempty
  mappend = liftA2 mappend

-- alternative idea, do this with writer
stmExampleW = do
  tv <- newTVarIO "xxx"
  join $ atomically $ execWriterT $ do
    val <- lift $ readTVar tv
    case val of
      "xxx" -> do
        lift $ writeTVar tv "foobar"
        tell $ print "it was xxx"
      "yyy" -> do
        lift $ writeTVar tv "quux"
        tell $ print "it was yyy"
    tell $ print "I finished"

pureExample p =
  runFinalV $ case p of
    True -> finalV "quux"
    False -> finalV "foobar"

testMissiles radarTVar launchKeyTVar radarNegativeCounter radarPositiveCounter keyMissingCounter launchCounter launchMissiles = do
 atomicJoinFinal $ do
   radarPositive <- readTVar radarTVar
   launchKeyInserted <- readTVar launchKeyTVar
   case radarPositive of
     False -> do
       modifyTVar radarNegativeCounter (+1)
       final $ print "No need for missiles, it's peaceful"
     True -> do
       modifyTVar radarPositiveCounter (+1)
       case launchKeyInserted of
         False -> do
           modifyTVar keyMissingCounter (+1)
           final $ print "No launch key, ignoring radar"
         True -> do
           modifyTVar launchCounter (+1)
           final $ launchMissiles
   -- final $ print "extra debug: state checking finished"

-- call like this: maybeReturningIO [('x', "foobar")] [('y', "xxx")]
maybeReturningIO :: [(Char, String)] -> [(Char, String)] -> IO ()
maybeReturningIO list1 list2 = do
  let maybeOp = do
        val1 <- lookup 'x' list1
        val2 <- lookup 'y' list2
        final $ print $ val1 ++ val2
  case maybeOp of
    Nothing -> print "lookups failed"
    Just op -> runFinalV op
