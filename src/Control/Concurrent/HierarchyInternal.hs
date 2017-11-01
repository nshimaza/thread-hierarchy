{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Control.Concurrent.HierarchyInternal
Copyright   : (c) Naoto Shimazaki 2017
License     : MIT (see the file LICENSE)

Maintainer  : https://github.com/nshimaza
Stability   : experimental

Internal implementations of Control.Concurrent.Hierarchy
-}
module Control.Concurrent.HierarchyInternal where

import           Control.Concurrent.Lifted      (ThreadId, forkWithUnmask,
                                                 killThread, myThreadId)
import           Control.Concurrent.MVar.Lifted (MVar, newEmptyMVar, newMVar,
                                                 putMVar, readMVar)
import           Control.Concurrent.STM.TVar    (TVar, modifyTVar', newTVarIO,
                                                 readTVarIO)
import           Control.Exception.Lifted       (AsyncException (ThreadKilled),
                                                 catch, finally, mask_)
import           Control.Monad.Base             (MonadBase, liftBase)
import           Control.Monad.STM              (atomically)
import           Control.Monad.Trans.Control    (MonadBaseControl)
import           Data.Foldable                  (forM_)
import           Data.Map.Strict                (Map, delete, elems, empty,
                                                 insert, keys)


{-|
    FinishMarker is created as empty MVar when a thread is created.
    It is automatically filled by () when the thread exits.
-}
newtype FinishMarker = FinishMarker (MVar ()) deriving (Eq)

{-|
    Mutable map containing thread ID and finish marker.
-}
newtype ThreadMap = ThreadMap (TVar (Map ThreadId FinishMarker))

{-|
    Create a new empty 'ThreadMap'.
-}
newThreadMap :: MonadBase IO m => m ThreadMap
newThreadMap = liftBase $ ThreadMap <$> newTVarIO empty

{-|
    Create a new thread and register it to given 'ThreadMap'.
-}
newChild
    :: MonadBaseControl IO m
    => ThreadMap           -- ^ ThreadMap where newly created thread will be registered.
    -> (ThreadMap -> m ()) -- ^ Action executed within the new thread.
    -> m ThreadId          -- ^ newChild returns ThreadId of created thread.
newChild brothers@(ThreadMap bMap) action = do
    finishMarker <- FinishMarker <$> newEmptyMVar
    children <- newThreadMap
    mask_ $ do
        child <- forkWithUnmask $ \unmask ->
            unmask (action children) `finally` cleanup finishMarker brothers children
        liftBase . atomically $ modifyTVar' bMap (insert child finishMarker)
        return child

{-|
    Kill all thread registered in given 'ThreadMap'.
    This version is exposed as API.  This is not called from cleanup routine.
    Thus it doesn't ignore asynchronous exceptions.
-}
killThreadHierarchy
    :: MonadBaseControl IO m
    => ThreadMap    -- ^ ThreadMap containing threads to be killed
    -> m ()
killThreadHierarchy (ThreadMap children) = do
    currentChildren <- liftBase $ readTVarIO children
    mapM_ killThread $ keys currentChildren
    remainingChildren <- liftBase $ readTVarIO children
    mapM_ (\(FinishMarker marker) -> readMVar marker) $ elems remainingChildren

{-|
    Kill all thread registered in given 'ThreadMap'.
    This internal version is only called from cleanup routine so
    this ignores ThreadKilled asynchronous exception.
-}
killThreadHierarchyInternal
    :: MonadBaseControl IO m
    => ThreadMap    -- ^ ThreadMap containing threads to be killed
    -> m ()
killThreadHierarchyInternal (ThreadMap children) = do
    {-
        Here we are going to kill all child threads for cleanup.  Because we are already under the way
        to terminate current thread, we don't want to be interrupted this process by ThreadKilled
        asynchronous exceptions.
        Though this routine runs under masked condition, there are two operation where asynchronous
        exception can interrupts them.  Those are killThread and readMVar.
        We just catch the exception, ignore it, then reattempt interrupted operation.
    -}
    currentChildren <- liftBase $ readTVarIO children
    forM_ (keys currentChildren) $ \child ->
        killThread child `catch` (\ThreadKilled -> killThread child)
    remainingChildren <- liftBase $ readTVarIO children
    forM_ (elems remainingChildren) $ \(FinishMarker marker) ->
        readMVar marker `catch` (\ThreadKilled -> readMVar marker)

{-|
    Thread clean up routine automatically installed by newChild.
    It first killThreadHierarchy all its child threads and unregister itself.
    This function is not an API function but for internal use only.
-}
cleanup :: MonadBaseControl IO m => FinishMarker -> ThreadMap -> ThreadMap -> m ()
cleanup (FinishMarker marker) (ThreadMap brotherMap) children = do
    killThreadHierarchyInternal children
    myThread <- myThreadId
    liftBase . atomically $ modifyTVar' brotherMap (delete myThread)
    putMVar marker ()   -- mark finish.
