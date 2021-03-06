{-# LANGUAGE LambdaCase, DeriveFunctor #-}
module Redeux.Command.Log (Log, reducer, log, trace, traceState) where

import Prelude hiding (log)
import Control.Monad.Trans (liftIO)
import Control.Monad.Free (iterM, liftF)

import qualified Redeux as Redeux

data Log grammer a next
  = Log String (Redeux.Command grammer a) (a -> next)
  | Trace (Redeux.Command grammer a) (a -> next)
  | TraceState (Redeux.Command grammer a) (a -> next)
  deriving (Functor)

reducer :: (Show a, Show state)
        => Redeux.Reducer grammer state a
        -> Redeux.Reducer (Log grammer a) state a
reducer subReducer = iterM $ \case
  Log msg command next -> do
    liftIO $ putStrLn msg
    subReducer command >>= next
  Trace command next -> do
    x <- subReducer command
    liftIO $ print x
    next x
  TraceState command next -> do
    Redeux.modifyStateM_ $ \s ->
      putStrLn "before" >> print s >> pure s
    x <- subReducer command
    Redeux.modifyStateM_ $ \s ->
      putStrLn "after" >> print s >> pure s
    next x

log :: String -> Redeux.Command grammer a -> Redeux.Command (Log grammer a) a
log str = liftF . flip (Log str) id

trace :: Redeux.Command grammer a -> Redeux.Command (Log grammer a) a
trace = liftF . flip Trace id

traceState :: Redeux.Command grammer a -> Redeux.Command (Log grammer a) a
traceState = liftF . flip TraceState id
