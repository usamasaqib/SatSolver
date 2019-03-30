{-
The design of this module was taken from 
the chapter on concurrency in the book Real World Haskell.
-}

module ThreadManagement 
(
  ThreadManager
, Status (Computing, Completed)
, newManager
, forkManaged
, findResult
) where

import Control.Concurrent
import qualified Control.Concurrent.MVar.Strict as Strict
import Control.Monad (join)
import Control.Exception (Exception, try)
import qualified Data.Map as M

data ThreadException = KilledByUncaughtException deriving (Eq, Show)

type Ticket = String

data Status = Completed [(Char, Bool)] | Computing

instance Exception ThreadException

newtype ThreadManager =
  Mgr (Strict.MVar (M.Map String (Strict.MVar [(Char, Bool)])))
  deriving Eq

-- Create a new threadManager
newManager :: IO ThreadManager
newManager = Mgr <$> newMVar M.empty

-- Create a new managed thread.
forkManaged :: ThreadManager -> Ticket -> String -> (String -> [(Char, Bool)]) ->  IO Ticket
forkManaged (Mgr mgr) ticket proposition solveSat =
  Strict.modifyMVar mgr $ \m ->
    do
      solMVar <- Strict.newEmptyMVar
      forkIO $ do
        result <- return $ solveSat proposition
        Strict.putMVar solMVar result
        
      return (M.insert ticket solMVar m, ticket)

-- Immediately return the status of a managed thread.
findResult :: ThreadManager -> Ticket -> IO (Maybe Status)
findResult (Mgr mgr) ticket = 
  Strict.modifyMVar mgr $ \m ->
    case M.lookup ticket m of
      Nothing -> return (m, Nothing)
      Just st -> Strict.tryTakeMVar st >>= \maybeBool ->
        case maybeBool of
          Nothing -> return (m, Just Computing)
          Just res -> return (M.delete ticket m, Just $ Completed res) 
