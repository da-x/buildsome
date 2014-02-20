module Lib.AsyncContext
  ( new, AsyncContext
  , spawn
  ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Data.IORef
import Data.IntMap (IntMap)
import Lib.IORef (atomicModifyIORef'_)
import qualified Control.Exception as E
import qualified Data.IntMap as IntMap

data AsyncContext = AsyncContext
  { ctxNextName :: IORef Int
  , ctxCancelActions :: MVar (IntMap (IO ()))
  }

new :: (AsyncContext -> IO a) -> IO a
new body = do
  nextNameRef <- newIORef 0
  cancelActionsVar <- newMVar IntMap.empty
  body (AsyncContext nextNameRef cancelActionsVar) `E.finally` do
    cancelActions <- readMVar cancelActionsVar
    sequence_ $ IntMap.elems cancelActions

spawn :: AsyncContext -> IO a -> IO (Async a)
spawn (AsyncContext nextNameRef cancelActionsVar) act = do
  name <- atomicModifyIORef' nextNameRef $ \name -> (name+1, name)
  modifyMVarMasked cancelActionsVar $ \cancelActions -> do
    actAsync <-
      async $
      act `E.finally` modifyMVar_ cancelActionsVar (return . IntMap.delete name)
    return
      ( IntMap.insert name (cancel actAsync) cancelActions
      , actAsync
      )