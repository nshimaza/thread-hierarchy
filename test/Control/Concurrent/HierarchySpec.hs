{-# LANGUAGE ScopedTypeVariables #-}

module Control.Concurrent.HierarchySpec where

import           Control.Concurrent      (killThread, threadDelay)
import           Control.Concurrent.MVar (MVar, isEmptyMVar, newEmptyMVar, readMVar, putMVar, takeMVar)
import           Control.Exception       (AsyncException(ThreadKilled), catch, throw)
import           Control.Monad           (forM_, void)
import           Data.Map.Strict         (toList)
import           Data.Typeable           (Typeable, typeOf)

import           Test.Hspec

import           Control.Concurrent.Hierarchy

instance Typeable a => Show (MVar a) where
    show mVar = show (typeOf mVar)

spec :: Spec
spec = do
    describe "newThreadMap" $ do
        it "returns a new empty ThreadMap" $ do
            (ThreadMap threadMap) <- newThreadMap
            currentChildren <- readMVar threadMap
            toList currentChildren `shouldBe` []

    describe "newChild" $ do
        it "forks a new thread and register it to given ThreadMap" $ do
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void $ newChild rootThreadMap $ \_ -> threadDelay (10 * 10^6)
            currentRootChildren <- readMVar rtMapMVar
            (length . toList) currentRootChildren `shouldBe` 1
            void $ newChild rootThreadMap $ \_ -> threadDelay (10 * 10^6)
            void $ newChild rootThreadMap $ \_ -> threadDelay (10 * 10^6)
            currentRootChildren <- readMVar rtMapMVar
            (length . toList) currentRootChildren `shouldBe` 3

        it "forks a thread which automatically cleanup registration of itself on exit" $ do
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void $ newChild rootThreadMap $ \_ -> return ()
            threadDelay (10 * 10^3)
            currentRootChildren <- readMVar rtMapMVar
            toList currentRootChildren `shouldBe` []
            void $ newChild rootThreadMap $ \_ -> return ()
            void $ newChild rootThreadMap $ \_ -> return ()
            void $ newChild rootThreadMap $ \_ -> return ()
            threadDelay (10 * 10^3)
            currentRootChildren <- readMVar rtMapMVar
            toList currentRootChildren `shouldBe` []

        it "forks a thread whihch automatically kills its child on normal exit" $ do
            parentExceptionMarker <- newEmptyMVar
            childExceptionMarker <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void $ newChild rootThreadMap $ \children -> do
                void $ newChild children $ \_ -> threadDelay (10 * 10^6)
                    `catch` \(e :: AsyncException) -> putMVar childExceptionMarker e
                threadDelay (10 * 10^3)
                    `catch` \(e :: AsyncException) -> putMVar parentExceptionMarker e
            currentRootChildren <- readMVar rtMapMVar
            (length . toList) currentRootChildren `shouldBe` 1
            threadDelay (20 * 10^3)
            currentRootChildren <- readMVar rtMapMVar
            toList currentRootChildren `shouldBe` []
            caughtExceptionByChild <- readMVar childExceptionMarker
            caughtExceptionByChild `shouldBe` ThreadKilled
            isParentNotKilled <- isEmptyMVar parentExceptionMarker
            isParentNotKilled `shouldBe` True

    describe "shutdown" $ do
        it "kills running thread in given ThreadMap" $ do
            exceptionMarker <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void $ newChild rootThreadMap $ \_ -> threadDelay (10 * 10^6)
                `catch` \(e :: AsyncException) -> putMVar exceptionMarker e
            currentRootChildren <- readMVar rtMapMVar
            (length . toList) currentRootChildren `shouldBe` 1
            threadDelay (10 * 10^3)
            shutdown rootThreadMap
            threadDelay (10 * 10^3)
            caughtException <- readMVar exceptionMarker
            caughtException `shouldBe` ThreadKilled
            currentRootChildren <- readMVar rtMapMVar
            toList currentRootChildren `shouldBe` []

        it "kills all running thread in given ThreadMap" $ do
            exceptionMarker1 <- newEmptyMVar
            exceptionMarker2 <- newEmptyMVar
            exceptionMarker3 <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void $ newChild rootThreadMap $ \_ -> threadDelay (10 * 10^6)
                `catch` \(e :: AsyncException) -> putMVar exceptionMarker1 e
            void $ newChild rootThreadMap $ \_ -> threadDelay (10 * 10^6)
                `catch` \(e :: AsyncException) -> putMVar exceptionMarker2 e
            void $ newChild rootThreadMap $ \_ -> threadDelay (10 * 10^6)
                `catch` \(e :: AsyncException) -> putMVar exceptionMarker3 e
            currentRootChildren <- readMVar rtMapMVar
            (length . toList) currentRootChildren `shouldBe` 3
            threadDelay (10 * 10^3)
            shutdown rootThreadMap
            threadDelay (10 * 10^3)
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
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void $ newChild rootThreadMap $ \children -> do
                void $ newChild children $ \_ -> threadDelay (10 * 10^6)
                    `catch` \(e :: AsyncException) -> putMVar childExceptionMarker e
                threadDelay (10 * 10^6)
                    `catch` \(e :: AsyncException) -> putMVar parentExceptionMarker e
            currentRootChildren <- readMVar rtMapMVar
            (length . toList) currentRootChildren `shouldBe` 1
            threadDelay (10 * 10^3)
            shutdown rootThreadMap
            threadDelay (10 * 10^3)
            currentRootChildren <- readMVar rtMapMVar
            toList currentRootChildren `shouldBe` []
            caughtExceptionByChild <- readMVar childExceptionMarker
            caughtExceptionByChild `shouldBe` ThreadKilled
            caughtExceptionByParent <- readMVar parentExceptionMarker
            caughtExceptionByParent `shouldBe` ThreadKilled

    describe "Finish marker MVar () inside of ThreadMap" $ do
        it "is filled by () on thread normal exit" $ do
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void $ newChild rootThreadMap $ \_ -> threadDelay (10 * 10^3)
            currentRootChildren <- readMVar rtMapMVar
            let (_, finishMarker) = head $ toList currentRootChildren
            mark <- takeMVar finishMarker
            mark `shouldBe` ()

        it "is filled by () on thread exit by kill" $ do
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void $ newChild rootThreadMap $ \_ -> threadDelay (10 * 10^6)
            currentRootChildren <- readMVar rtMapMVar
            let (threadID, finishMarker) = head $ toList currentRootChildren
            killThread threadID
            mark <- takeMVar finishMarker
            mark `shouldBe` ()

        it "is available after thread normal exit" $ do
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void $ newChild rootThreadMap $ \_ -> threadDelay (10 * 10^3)
            currentRootChildren <- readMVar rtMapMVar
            let (_, finishMarker) = head $ toList currentRootChildren
            threadDelay (20 * 10^3)
            currentRootChildren <- readMVar rtMapMVar
            toList currentRootChildren `shouldBe` []
            mark <- takeMVar finishMarker
            mark `shouldBe` ()

        it "is available after the thread was killed" $ do
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void $ newChild rootThreadMap $ \_ -> threadDelay (10 * 10^6)
            currentRootChildren <- readMVar rtMapMVar
            let (threadID, finishMarker) = head $ toList currentRootChildren
            killThread threadID
            threadDelay (10 * 10^3)
            currentRootChildren <- readMVar rtMapMVar
            toList currentRootChildren `shouldBe` []
            mark <- takeMVar finishMarker
            mark `shouldBe` ()

        it "works with many threads in normal exit scenario" $ do
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            forM_ [1..10000] . const . newChild rootThreadMap $ \_ -> threadDelay (100 * 10^3)
            currentRootChildren <- readMVar rtMapMVar
            let childrenList = toList currentRootChildren
            length childrenList `shouldBe` 10000
            threadDelay (1 * 10^6)
            remainingRootChildren <- readMVar rtMapMVar
            toList remainingRootChildren `shouldBe` []
            forM_ childrenList $ \(_, finishMarker) -> do
                mark <- takeMVar finishMarker
                mark `shouldBe` ()

        it "works with many threads in killed exit scenario" $ do
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            forM_ [1..10000] . const . newChild rootThreadMap $ \_ -> threadDelay (10 * 10^6)
            currentRootChildren <- readMVar rtMapMVar
            let childrenList = toList currentRootChildren
            length childrenList `shouldBe` 10000
            forM_ childrenList $ \(threadID, _) -> killThread threadID
            threadDelay (1 * 10^6)
            remainingRootChildren <- readMVar rtMapMVar
            toList remainingRootChildren `shouldBe` []
            forM_ childrenList $ \(_, finishMarker) -> do
                mark <- takeMVar finishMarker
                mark `shouldBe` ()

        it "kill terminated thread" $ do
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void $ newChild rootThreadMap $ \_ -> threadDelay (10 * 10^3)
            currentRootChildren <- readMVar rtMapMVar
            let (threadID, finishMarker) = head $ toList currentRootChildren
            threadDelay (30 * 10^3)
            killThread threadID
            remainingRootChildren <- readMVar rtMapMVar
            toList remainingRootChildren `shouldBe` []

