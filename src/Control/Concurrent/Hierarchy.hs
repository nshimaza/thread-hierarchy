module Control.Concurrent.Hierarchy
    (
      ThreadMap(..)
    , newThreadMap
    , newChild
    , shutdown
    ) where

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

newThreadMap :: IO ThreadMap
newThreadMap = ThreadMap <$> newMVar empty

newChild :: ThreadMap -> (ThreadMap -> IO ()) -> IO ThreadId
newChild brothers@(ThreadMap bMap) action = do
    finishFlag <- newEmptyMVar
    children <- newThreadMap
    mask_ $ do
        child <- forkIOWithUnmask $ \unmask ->
            (unmask (action children)) `finally` (cleanup brothers children)
        takeMVar bMap >>= putMVar bMap . insert child finishFlag
        return child

shutdown :: ThreadMap -> IO ()
shutdown (ThreadMap children) = do
    currentChildren <- readMVar children
    mapM_ (killThread . fst) $ toList currentChildren
    remainingChildren <- readMVar children
    mapM_ (takeMVar . snd) $ toList remainingChildren

cleanup :: ThreadMap -> ThreadMap -> IO ()
cleanup (ThreadMap brotherMap) children = do
    shutdown children
    myThread <- myThreadId
    bMap <- readMVar brotherMap
    case lookup myThread bMap of
        (Just bMVar)    -> do
            putMVar bMVar ()
            takeMVar brotherMap >>= putMVar brotherMap . delete myThread
        Nothing         -> return ()
