module Control.Concurrent.HierarchyInternal where

import           Control.Concurrent      (ThreadId, forkIOWithUnmask,
                                          killThread, myThreadId)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar,
                                          readMVar, takeMVar)
import           Control.Exception       (finally, mask_)
import           Data.Map.Strict         (Map, delete, empty, insert,
                                          toList)

{-|
    FinishMarker is created as empty MVar when a thread is created.
    It is automatically filled by () when the thread exits.
-}
newtype FinishMarker = FinishMarker (MVar ()) deriving (Eq)

{-|
    Mutable map containing thread ID and finish marker.
-}
-- newtype ThreadMap = ThreadMap (MVar (Map ThreadId (MVar ())))
newtype ThreadMap = ThreadMap (MVar (Map ThreadId FinishMarker))

{-|
    Create a new empty 'ThreadMap'.
-}
newThreadMap :: IO ThreadMap
newThreadMap = ThreadMap <$> newMVar empty

{-|
    Create a new thread and register it to given 'ThreadMap'.
-}
newChild
    :: ThreadMap            -- ^ ThreadMap where newly created thread will be registered.
    -> (ThreadMap -> IO ()) -- ^ Action executed within the new thread.
    -> IO ThreadId          -- ^ newChild returns ThreadId of created thread.
newChild brothers@(ThreadMap bMap) action = do
--     finishMarker <- newEmptyMVar
    finishMarker <- newFinishMarker
    children <- newThreadMap
    mask_ $ do
        child <- forkIOWithUnmask $ \unmask ->
            (unmask (action children)) `finally` (cleanup finishMarker brothers children)
        takeMVar bMap >>= putMVar bMap . insert child finishMarker
        return child

{-|
    Kill all thread registered in given 'ThreadMap'.
-}
shutdown
    :: ThreadMap    -- ^ ThreadMap containing threads to be killed
    -> IO ()
shutdown (ThreadMap children) = do
    currentChildren <- readMVar children
    mapM_ (killThread . fst) $ toList currentChildren
    remainingChildren <- readMVar children
    mapM_ (waitFinish . snd) $ toList remainingChildren

newFinishMarker :: IO FinishMarker
newFinishMarker = FinishMarker <$> newEmptyMVar

markFinish :: FinishMarker -> IO ()
markFinish (FinishMarker marker) = putMVar marker ()

waitFinish :: FinishMarker -> IO ()
waitFinish (FinishMarker marker) = takeMVar marker

{-|
    Thread clean up routine automatically installed by newChild.
    It shutdowns all its child threads and unregister itself.
    This function is not an API function but for internal use only.
-}
cleanup :: FinishMarker -> ThreadMap -> ThreadMap -> IO ()
cleanup finishMarker (ThreadMap brotherMap) children = do
    shutdown children
    myThread <- myThreadId
    takeMVar brotherMap >>= putMVar brotherMap . delete myThread
    markFinish finishMarker
