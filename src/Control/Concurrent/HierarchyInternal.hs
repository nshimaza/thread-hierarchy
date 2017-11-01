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
    finishMarker <- newFinishMarker
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
    Kill a child thread.  Only used by killThreadHierarchy routine internally.

    killThread can be interrupted by ThreadKilled exception thrown from the parent thread.
    However, because we are here, this thread is being cleaned up.
    So we can just ignore it and attempt killThread again.
    Because ThreadKilled is thrown only one time from the parent, we don't try to catch it again.
-}
killChild :: MonadBaseControl IO m => ThreadId -> m ()
killChild child = killThread child `catch` (\ThreadKilled -> killThread child)

{-|
    Create new empty finish marker.  Internal use only.
-}
newFinishMarker :: MonadBase IO m => m FinishMarker
newFinishMarker = FinishMarker <$> newEmptyMVar

{-|
    Filling MVar of finish marker to mark thread finished.  Only used by cleanup routine internally.
-}
markFinish :: MonadBase IO m => FinishMarker -> m ()
markFinish (FinishMarker marker) = putMVar marker ()

{-|
    Wait for finish marker marked.  Only used by killThreadHierarchy routine internally.

    readMVar can be interrupted by ThreadKilled exception thrown from the parent thread.
    However, because we are here, this thread is being cleaned up.
    So we can just ignore it and attempt killThread again.
    Because ThreadKilled is thrown only one time from the parent, we don't try to catch it again.
-}
waitFinish :: MonadBaseControl IO m => FinishMarker -> m ()
waitFinish (FinishMarker marker) = readMVar marker `catch` (\ThreadKilled -> readMVar marker)

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
    currentChildren <- liftBase $ readTVarIO children
    mapM_ killChild $ keys currentChildren
    remainingChildren <- liftBase $ readTVarIO children
    mapM_ waitFinish $ elems remainingChildren

{-|
    Thread clean up routine automatically installed by newChild.
    It first killThreadHierarchy all its child threads and unregister itself.
    This function is not an API function but for internal use only.
-}
cleanup :: MonadBaseControl IO m => FinishMarker -> ThreadMap -> ThreadMap -> m ()
cleanup finishMarker (ThreadMap brotherMap) children = do
    killThreadHierarchyInternal children
    myThread <- myThreadId
    liftBase . atomically $ modifyTVar' brotherMap (delete myThread)
    markFinish finishMarker
