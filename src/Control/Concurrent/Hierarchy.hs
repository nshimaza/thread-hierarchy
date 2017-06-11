{-|
Module      : Control.Concurrent.Hierarchy
Copyright   : (c) Naoto Shimazaki 2017
License     : MIT (see the file LICENSE)

Maintainer  : https://github.com/nshimaza
Stability   : experimental

Managing Haskell threads in hierarchical manner.

Threads created by newChild guarantee automatic cleanup on its exit regardless normal exit
or cancellation by asynchronous exception.

In order to this module works properly, user must ensure following rules.

* User provided thread handler must accept 'ThreadMap' as its first argument.
* When new thread created by newChild, handler receives newly created empty 'ThreadMap'.
* When the user provided handler creates its child thread, it must use newChild.
* At the same time, the handler must pass the 'ThreadMap' received via its argument to newChild.

'ThreadMap' is mutable map holding live threads.  Each threads managed by this module has its
own 'ThreadMap' instance.  Each 'ThreadMap' keeps live "child" threads for future cleanup on exit.
Populating 'ThreadMap' is done by newChild automatically.
Actually the function newChild accept a 'ThreadMap' then mutate it.

At the same time newChild create a new empty 'ThreadMap' for newly created thread and pass it
to user provided handler of the new thread.
Also newChild automatically install cleanup routine which kill all threads living in the new
'ThreadMap' created for the thread.
The cleanup routine also unregister itself from 'ThreadMap' of parent.

In order to work this properly, user provided thread handler must use newChild with 'ThreadMap'
given via its argument when it creates its child so that cleanup routine can terminate
its children properly.


=== Example

When you create the first thread managed by this module, create a new empty 'ThreadMap' then call
newChild with it.  The newCall automatically install cleanup routine to the handler you provided.

@
createRootThread :: IO ThreadId
createRootThread = do
    rootThreadMap <- newThreadMap
    threadID <- newChild rootThreadMap rootThreadHandler
    return threadID
@

When a thread managed by this module creates its child thread, call newChild with 'TreadMap'
received via handlers argument.

@
rootThreadHandler :: ThreadMap -> IO ()
rootThreadHandler myChildrenThreadMap = do
    void $ newChild myChildrenThreadMap $ \grandChildrenThreadMap -> do
        yourCode
        return ()
@

=== Limitation

Currently, unlike async function, this module is not designed to back any return value
from child thread to parent thread.  This module focuses on guaranteed cleanup on thread termination.

-}

module Control.Concurrent.Hierarchy
    (
    -- * Types
      ThreadMap(..)
    -- * Functions
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

{-|
    Mutable map containing thread ID and finish marker.
-}
newtype ThreadMap = ThreadMap (MVar (Map ThreadId (MVar ())))

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
    finishFlag <- newEmptyMVar
    children <- newThreadMap
    mask_ $ do
        child <- forkIOWithUnmask $ \unmask ->
            (unmask (action children)) `finally` (cleanup brothers children)
        takeMVar bMap >>= putMVar bMap . insert child finishFlag
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
    mapM_ (takeMVar . snd) $ toList remainingChildren

{-|
    Thread clean up routine automatically installed by newChild.
    It shutdowns all its child threads and unregister itself.
    This function is not an API function but for internal use only.
-}
cleanup :: ThreadMap -> ThreadMap -> IO ()
cleanup (ThreadMap brotherMap) children = do
    shutdown children
    myThread <- myThreadId
    bMap <- readMVar brotherMap
    case lookup myThread bMap of
        (Just bMVar)    -> do
            takeMVar brotherMap >>= putMVar brotherMap . delete myThread
            putMVar bMVar ()
        Nothing         -> return ()
