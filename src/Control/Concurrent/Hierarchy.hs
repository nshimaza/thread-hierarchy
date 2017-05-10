module Control.Concurrent.Hierarchy where

import           Prelude                 hiding (lookup)

import           Control.Concurrent      (ThreadId, forkIOWithUnmask,
                                          killThread, myThreadId)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar,
                                          readMVar, takeMVar)
import           Control.Exception       (finally, mask_)
import           Control.Monad           (void)
import           Data.Map.Strict         (Map, delete, empty, insert, lookup,
                                          toList)


newtype ThreadMap = ThreadMap (MVar (Map ThreadId (MVar ())))

newRoot :: (ThreadMap -> IO ()) -> IO ThreadMap
newRoot action = mask_ $ do
    rootChildren <- ThreadMap <$> newMVar empty
    void $ newChild rootChildren action
    return rootChildren

shutdownRoot :: ThreadMap -> IO ()
shutdownRoot (ThreadMap rootChildren) = do
    currentChildren <- readMVar rootChildren
    mapM_ (killThread . fst) $ toList currentChildren
    remainingChildren <- readMVar rootChildren
    mapM_ (takeMVar . snd) $ toList remainingChildren

newChild :: ThreadMap -> (ThreadMap -> IO ()) -> IO ThreadId
newChild brothers@(ThreadMap bMap) action = do
    finishFlag <- newEmptyMVar
    children <- ThreadMap <$> newMVar empty
    mask_ $ do
        child <- forkIOWithUnmask $ \unmask ->
            finally (unmask (action children)) (shutdownChildren brothers children)
        takeMVar bMap >>= putMVar bMap . insert child finishFlag
        return child

shutdownChildren :: ThreadMap -> ThreadMap -> IO ()
shutdownChildren (ThreadMap brotherMap) (ThreadMap children) = do
    currentChildren <- readMVar children
    mapM_ (killThread . fst) $ toList currentChildren
    remainingChildren <- readMVar children
    mapM_ (takeMVar . snd) $ toList remainingChildren
    myThread <- myThreadId
    bMap <- readMVar brotherMap
    case lookup myThread bMap of
        (Just bMVar)    -> do
            putMVar bMVar ()
            takeMVar brotherMap >>= putMVar brotherMap . delete myThread
        Nothing         -> return ()
