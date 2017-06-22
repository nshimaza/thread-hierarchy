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
                                                 putMVar, readMVar, takeMVar)
import           Control.Exception.Lifted       (finally, mask_)
import           Control.Monad.Base             (MonadBase)
import           Control.Monad.Trans.Control    (MonadBaseControl)
import           Data.Map.Strict                (Map, delete, empty, insert,
                                                 toList)


{-|
    FinishMarker is created as empty MVar when a thread is created.
    It is automatically filled by () when the thread exits.
-}
newtype FinishMarker = FinishMarker (MVar ()) deriving (Eq)

{-|
    Mutable map containing thread ID and finish marker.
-}
newtype ThreadMap = ThreadMap (MVar (Map ThreadId FinishMarker))

{-|
    Create a new empty 'ThreadMap'.
-}
newThreadMap :: MonadBase IO m => m ThreadMap
newThreadMap = ThreadMap <$> newMVar empty

{-|
    Create a new thread and register it to given 'ThreadMap'.
-}
newChild
    :: MonadBaseControl IO m
    => ThreadMap            -- ^ ThreadMap where newly created thread will be registered.
    -> (ThreadMap -> m ()) -- ^ Action executed within the new thread.
    -> m ThreadId          -- ^ newChild returns ThreadId of created thread.
newChild brothers@(ThreadMap bMap) action = do
    finishMarker <- newFinishMarker
    children <- newThreadMap
    mask_ $ do
        child <- forkWithUnmask $ \unmask ->
            unmask (action children) `finally` cleanup finishMarker brothers children
        takeMVar bMap >>= putMVar bMap . insert child finishMarker
        return child

{-|
    Kill all thread registered in given 'ThreadMap'.
-}
shutdown
    :: MonadBase IO m
    => ThreadMap    -- ^ ThreadMap containing threads to be killed
    -> m ()
shutdown (ThreadMap children) = do
    currentChildren <- readMVar children
    mapM_ (killThread . fst) $ toList currentChildren
    remainingChildren <- readMVar children
    mapM_ (waitFinish . snd) $ toList remainingChildren

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
    Wait for finish marker marked.  Only used by shutdown routine internally.
-}
waitFinish :: MonadBase IO m => FinishMarker -> m ()
waitFinish (FinishMarker marker) = readMVar marker

{-|
    Thread clean up routine automatically installed by newChild.
    It shutdowns all its child threads and unregister itself.
    This function is not an API function but for internal use only.
-}
cleanup :: MonadBase IO m => FinishMarker -> ThreadMap -> ThreadMap -> m ()
cleanup finishMarker (ThreadMap brotherMap) children = do
    shutdown children
    myThread <- myThreadId
    takeMVar brotherMap >>= putMVar brotherMap . delete myThread
    markFinish finishMarker
