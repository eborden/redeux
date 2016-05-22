{-# LANGUAGE LambdaCase, DeriveFunctor #-}
module Redeux.Command.Async (Control, reducer, async, sync) where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Control.Monad.Free (iterM, liftF)

import qualified Redeux as Redeux

data Control grammer a next
  = Sync (Redeux.Command grammer a) (a -> next)
  | Async (Redeux.Command grammer a) next
  deriving (Functor)

reducer :: Redeux.Reducer grammer state a
            -> Redeux.Reducer (Control grammer a) state a
reducer subReducer = iterM $ \case
  Sync command next -> subReducer command >>= next
  Async command next -> do
    env <- Redeux.dupEnv
    void . liftIO . forkIO . env $ do
      void $ subReducer command
      Redeux.revalidateUI
    next

async :: Redeux.Command grammer () -> Redeux.Command (Control grammer ()) ()
async = liftF . flip Async ()

sync :: Redeux.Command grammer a -> Redeux.Command (Control grammer a) a
sync = liftF . flip Sync id
