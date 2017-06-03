{-# LANGUAGE ScopedTypeVariables #-}

module Control.Concurrent.HierarchySpec where

import           Control.Concurrent      (threadDelay)
import           Control.Concurrent.MVar (MVar, isEmptyMVar, newEmptyMVar, readMVar, putMVar)
import           Control.Exception       (AsyncException(ThreadKilled), catch, throw)
import           Control.Monad           (void)
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
            threadDelay (1 * 10^4)
            currentRootChildren <- readMVar rtMapMVar
            toList currentRootChildren `shouldBe` []
            void $ newChild rootThreadMap $ \_ -> return ()
            void $ newChild rootThreadMap $ \_ -> return ()
            void $ newChild rootThreadMap $ \_ -> return ()
            threadDelay (1 * 10^4)
            currentRootChildren <- readMVar rtMapMVar
            toList currentRootChildren `shouldBe` []

        it "forks a thread whihch automatically kills its child on normal exit" $ do
            parentExceptionMarker <- newEmptyMVar
            childExceptionMarker <- newEmptyMVar
            rootThreadMap@(ThreadMap rtMapMVar) <- newThreadMap
            void $ newChild rootThreadMap $ \children -> do
                void $ newChild children $ \_ -> threadDelay (10 * 10^6)
                    `catch` \(e :: AsyncException) -> putMVar childExceptionMarker e
                threadDelay (1 * 10^4)
                    `catch` \(e :: AsyncException) -> putMVar parentExceptionMarker e
            currentRootChildren <- readMVar rtMapMVar
            (length . toList) currentRootChildren `shouldBe` 1
            threadDelay (2 * 10^4)
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
            threadDelay (1 * 10^4)
            shutdown rootThreadMap
            threadDelay (1 * 10^4)
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
            threadDelay (1 * 10^4)
            shutdown rootThreadMap
            threadDelay (1 * 10^4)
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
            threadDelay (1 * 10^4)
            shutdown rootThreadMap
            threadDelay (1 * 10^4)
            currentRootChildren <- readMVar rtMapMVar
            toList currentRootChildren `shouldBe` []
            caughtExceptionByChild <- readMVar childExceptionMarker
            caughtExceptionByChild `shouldBe` ThreadKilled
            caughtExceptionByParent <- readMVar parentExceptionMarker
            caughtExceptionByParent `shouldBe` ThreadKilled
