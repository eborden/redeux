{-# LANGUAGE TupleSections #-}
module Redeux.Core where

import Data.IORef
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Monad (forever, void)
import Control.Monad.Free
import Control.Monad.Trans.Reader
import Control.Monad.Trans

type Command                      = Free
type Env state result             = ReaderT (IORef state) IO result
type Reducer grammer state result = Command grammer result -> Env state result
type Sink grammer                 = Command grammer () -> IO () -- This should really be a write only chan
type UserInterface grammer state  = Sink grammer -> state -> IO () -- This can likely be refined

redeux :: Show state => state -> Reducer grammer state () -> UserInterface grammer state -> IO (state -> IO (), Chan state)
redeux state reducer interface = do
  stateRef <- newIORef state
  sink <- newChan
  stateChan <- newChan
  let runInterface = interface (writeChan sink) =<< readIORef stateRef

  -- listen and rerun the interface when commands are run
  void . forkIO . forever $ do
    command <- readChan sink
    runReaderT (reducer command) stateRef
    runInterface
    writeChan stateChan =<< readIORef stateRef

  -- initial state
  interface (writeChan sink) state

  -- return the state channel and state mutator for instrumentation
  let mutateState x = writeIORef stateRef x >> runInterface
  (mutateState, ) <$> dupChan stateChan

getState :: Env s s
getState = liftIO . readIORef =<< ask

writeState :: s -> Env s ()
writeState s = liftIO . flip writeIORef s =<< ask

withState :: (s -> Env s r) -> Env s r
withState f = f =<< liftIO . readIORef =<< ask

modifyState :: (s -> s) -> Env s ()
modifyState f = liftIO . flip atomicModifyIORef' ((, ()) . f) =<< ask
