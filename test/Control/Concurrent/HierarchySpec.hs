{-# LANGUAGE ScopedTypeVariables #-}

module Control.Concurrent.HierarchySpec where

import           Control.Concurrent      (killThread, threadDelay)
import           Control.Concurrent.MVar (MVar, isEmptyMVar, newEmptyMVar, readMVar, putMVar, takeMVar)
import           Control.Exception       (AsyncException(ThreadKilled), catch)
import           Control.Monad           (forM_, forM, void, unless)
import           Data.Map.Strict         (toList)
import           Data.Typeable           (Typeable, typeOf)

import           Test.Hspec

import           Control.Concurrent.HierarchyInternal

instance Typeable a => Show (MVar a) where
    show mVar = show (typeOf mVar)

instance Show FinishMarker where
    show (FinishMarker mVar) = "FinishMarker" ++ show mVar

spec :: Spec
spec = do
    describe "newThreadMap" $
        it "returns a new empty ThreadMap" $ do
            (ThreadMap threadMap) <- newThreadMap
            currentChildren <- readMVar threadMap
            toList currentChildren `shouldBe` []

    describe "newChild" $ do
        it "forks a new thread and register it to given ThreadMap" $ do
            blocker <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \_ -> takeMVar blocker
            oneRootChildren <- readMVar rtMapMVar
            (length . toList) oneRootChildren `shouldBe` 1
            void . newChild rootThreadMap $ \_ -> takeMVar blocker
            void . newChild rootThreadMap $ \_ -> takeMVar blocker
            threeRootChildren <- readMVar rtMapMVar
            (length . toList) threeRootChildren `shouldBe` 3

        it "forks a thread which automatically cleanup registration of itself on exit" $ do
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \_ -> return ()
            threadDelay (10 * 1000)
            oneRootChildren <- readMVar rtMapMVar
            toList oneRootChildren `shouldBe` []
            void . newChild rootThreadMap $ \_ -> return ()
            void . newChild rootThreadMap $ \_ -> return ()
            void . newChild rootThreadMap $ \_ -> return ()
            threadDelay (10 * 1000)
            threeRootChildren <- readMVar rtMapMVar
            toList threeRootChildren `shouldBe` []

        it "forks a thread whihch automatically kills its child on normal exit" $ do
            parentExceptionMarker <- newEmptyMVar
            childExceptionMarker <- newEmptyMVar
            blocker <- newEmptyMVar
            trigger <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \children -> do
                void . newChild children $ \_ -> takeMVar blocker
                    `catch` \(e :: AsyncException) -> putMVar childExceptionMarker e
                takeMVar trigger
                    `catch` \(e :: AsyncException) -> putMVar parentExceptionMarker e
            runningRootChildren <- readMVar rtMapMVar
            (length . toList) runningRootChildren `shouldBe` 1
            threadDelay (10 * 1000)
            putMVar trigger ()
            threadDelay (10 * 1000)
            finishedRootChildren <- readMVar rtMapMVar
            toList finishedRootChildren `shouldBe` []
            caughtExceptionByChild <- readMVar childExceptionMarker
            caughtExceptionByChild `shouldBe` ThreadKilled
            isParentNotKilled <- isEmptyMVar parentExceptionMarker
            isParentNotKilled `shouldBe` True

    describe "killThreadHierarchy" $ do
        it "kills running thread in given ThreadMap" $ do
            exceptionMarker <- newEmptyMVar
            blocker <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \_ -> takeMVar blocker
                `catch` \(e :: AsyncException) -> putMVar exceptionMarker e
            currentRootChildren <- readMVar rtMapMVar
            (length . toList) currentRootChildren `shouldBe` 1
            threadDelay (10 * 1000)
            killThreadHierarchy rootThreadMap
            threadDelay (10 * 1000)
            caughtException <- readMVar exceptionMarker
            caughtException `shouldBe` ThreadKilled
            remainingRootChildren <- readMVar rtMapMVar
            toList remainingRootChildren `shouldBe` []

        it "kills all running thread in given ThreadMap" $ do
            exceptionMarker1 <- newEmptyMVar
            exceptionMarker2 <- newEmptyMVar
            exceptionMarker3 <- newEmptyMVar
            blocker <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \_ -> takeMVar blocker
                `catch` \(e :: AsyncException) -> putMVar exceptionMarker1 e
            void . newChild rootThreadMap $ \_ -> takeMVar blocker
                `catch` \(e :: AsyncException) -> putMVar exceptionMarker2 e
            void . newChild rootThreadMap $ \_ -> takeMVar blocker
                `catch` \(e :: AsyncException) -> putMVar exceptionMarker3 e
            currentRootChildren <- readMVar rtMapMVar
            (length . toList) currentRootChildren `shouldBe` 3
            threadDelay (10 * 1000)
            killThreadHierarchy rootThreadMap
            threadDelay (10 * 1000)
            caughtException1 <- readMVar exceptionMarker1
            caughtException2 <- readMVar exceptionMarker2
            caughtException3 <- readMVar exceptionMarker3
            caughtException1 `shouldBe` ThreadKilled
            caughtException2 `shouldBe` ThreadKilled
            caughtException3 `shouldBe` ThreadKilled
            currentRootChildren <- readMVar rtMapMVar
            toList currentRootChildren `shouldBe` []

        it "kills entire hierarchy of threads" $ do
            parentExceptionMarker <- newEmptyMVar
            childExceptionMarker <- newEmptyMVar
            blocker <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \children -> do
                void . newChild children $ \_ -> takeMVar blocker
                    `catch` \(e :: AsyncException) -> putMVar childExceptionMarker e
                takeMVar blocker
                    `catch` \(e :: AsyncException) -> putMVar parentExceptionMarker e
            currentRootChildren <- readMVar rtMapMVar
            (length . toList) currentRootChildren `shouldBe` 1
            threadDelay (10 * 1000)
            killThreadHierarchy rootThreadMap
            threadDelay (10 * 1000)
            currentRootChildren <- readMVar rtMapMVar
            toList currentRootChildren `shouldBe` []
            caughtExceptionByChild <- readMVar childExceptionMarker
            caughtExceptionByChild `shouldBe` ThreadKilled
            caughtExceptionByParent <- readMVar parentExceptionMarker
            caughtExceptionByParent `shouldBe` ThreadKilled

    describe "Finish marker MVar () inside of ThreadMap" $ do
        it "is filled by () on thread normal exit" $ do
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \_ -> threadDelay (10 * 1000)
            currentRootChildren <- readMVar rtMapMVar
            let (_, FinishMarker markerMVar) = head $ toList currentRootChildren
            mark <- takeMVar markerMVar
            mark `shouldBe` ()

        it "is filled by () on thread exit by kill" $ do
            blocker <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \_ -> takeMVar blocker
            currentRootChildren <- readMVar rtMapMVar
            let (threadID, FinishMarker markerMVar) = head $ toList currentRootChildren
            killThread threadID
            mark <- takeMVar markerMVar
            mark `shouldBe` ()

        it "is available after thread normal exit" $ do
            trigger <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \_ -> takeMVar trigger
            currentRootChildren <- readMVar rtMapMVar
            let (_, FinishMarker markerMVar) = head $ toList currentRootChildren
            putMVar trigger ()
            threadDelay (10 * 1000)
            currentRootChildren <- readMVar rtMapMVar
            toList currentRootChildren `shouldBe` []
            mark <- takeMVar markerMVar
            mark `shouldBe` ()

        it "is available after the thread was killed" $ do
            blocker <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \_ -> takeMVar blocker
            currentRootChildren <- readMVar rtMapMVar
            let (threadID, FinishMarker markerMVar) = head $ toList currentRootChildren
            killThread threadID
            threadDelay (10 * 1000)
            currentRootChildren <- readMVar rtMapMVar
            toList currentRootChildren `shouldBe` []
            mark <- takeMVar markerMVar
            mark `shouldBe` ()

        it "works with many threads in normal exit scenario" $ do
            let volume = 10000
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            triggers <- forM [1..volume] $ \_ -> do
                trigger <- newEmptyMVar
                void . newChild rootThreadMap $ \_ -> takeMVar trigger
                return trigger
            currentRootChildren <- readMVar rtMapMVar
            let childrenList = toList currentRootChildren
            length childrenList `shouldBe` volume

            forM_ triggers $ \trigger -> putMVar trigger ()
            let waitForCleanup = do
                    remaining <- readMVar rtMapMVar
                    unless (toList remaining == []) $ threadDelay (10 * 1000) >> waitForCleanup
            waitForCleanup
            remainingRootChildren <- readMVar rtMapMVar
            toList remainingRootChildren `shouldBe` []
            forM_ childrenList $ \(_, FinishMarker markerMVar) -> do
                mark <- takeMVar markerMVar
                mark `shouldBe` ()

        it "works with many threads in killed exit scenario" $ do
            let volume = 10000
            blocker <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            forM_ [1..volume] . const . newChild rootThreadMap $ \_ -> takeMVar blocker
            currentRootChildren <- readMVar rtMapMVar
            let childrenList = toList currentRootChildren
            length childrenList `shouldBe` volume
            forM_ childrenList $ \(threadID, _) -> killThread threadID
            let waitForCleanup = do
                    remaining <- readMVar rtMapMVar
                    unless (toList remaining == []) $ threadDelay (10 * 1000) >> waitForCleanup
            waitForCleanup
            remainingRootChildren <- readMVar rtMapMVar
            toList remainingRootChildren `shouldBe` []
            forM_ childrenList $ \(_, FinishMarker markerMVar) -> do
                mark <- takeMVar markerMVar
                mark `shouldBe` ()

    describe "Overkill" $ do
        it "Killing normally exited thread doesn't take any effect" $ do
            trigger <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \_ -> takeMVar trigger
            currentRootChildren <- readMVar rtMapMVar
            let (threadID, _) = head $ toList currentRootChildren
            threadDelay (10 * 1000)
            putMVar trigger ()
            threadDelay (10 * 1000)
            killThread threadID
            remainingRootChildren <- readMVar rtMapMVar
            toList remainingRootChildren `shouldBe` []

        it "Killing already killed thread doesn't take any effect" $ do
            blocker <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void . newChild rootThreadMap $ \_ -> takeMVar blocker
            currentRootChildren <- readMVar rtMapMVar
            let (threadID, _) = head $ toList currentRootChildren
            threadDelay (10 * 1000)
            killThread threadID
            threadDelay (10 * 1000)
            remainingRootChildren <- readMVar rtMapMVar
            toList remainingRootChildren `shouldBe` []
            killThread threadID
            threadDelay (10 * 1000)
            remainingRootChildren <- readMVar rtMapMVar
            toList remainingRootChildren `shouldBe` []
