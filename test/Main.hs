{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Singleton
import Data.Foldable (traverse_)
import Data.IORef
import Data.List.Extra
import Test.Hspec
import Unique

newtype MyGlobal = MyGlobal (IORef Int)

instance Singleton MyGlobal where
  initialize = MyGlobal <$> newIORef 0

setMyGlobal :: IO ()
setMyGlobal = do
  MyGlobal g <- only
  writeIORef g 5

resetMyGlobal :: IO ()
resetMyGlobal = do
  MyGlobal g <- only
  writeIORef g 0

newtype TestGlobal = TestGlobal ()

instance Singleton TestGlobal where
  initialize = do
    MyGlobal g <- only
    atomicModifyIORef' g (\x -> (x + 1, ()))
    pure (TestGlobal ())

data TestException = TestException deriving (Eq, Show)

instance Exception TestException

newtype FailingGlobal = FailingGlobal ()

instance Singleton FailingGlobal where
  initialize = do
    MyGlobal g <- only
    x <- atomicModifyIORef' g (1,)
    case x of
      0 -> throwIO TestException
      _ -> pure (FailingGlobal ())

main :: IO ()
main = hspec $ do
  it "tests that unique values are distinct" $ do
    uniqs <- replicateM 1000 newUnique
    anySame uniqs `shouldBe` False
  it "tests global IORef" $ do
    setMyGlobal
    MyGlobal ref <- only
    readIORef ref `shouldReturn` 5
  it "tests threaded calls to only" $ do
    resetMyGlobal
    counter <- newIORef (0 :: Int)
    vars <- replicateM 1000 $ do
      var <- newEmptyMVar
      _ <-
        forkFinally
          ( do
              yield
              atomicModifyIORef' counter (\x -> (x + 1, ()))
              yield
              TestGlobal () <- only
              yield
              atomicModifyIORef' counter (\x -> (x + 1, ()))
              yield
          )
          (const (putMVar var ()))
      pure var
    traverse_ takeMVar vars
    MyGlobal ref <- only
    readIORef ref `shouldReturn` 1
    readIORef counter `shouldReturn` 2000
  it "tests exceptions in initializers" $ do
    resetMyGlobal
    (only :: IO FailingGlobal) `shouldThrow` (== TestException)
    (only :: IO FailingGlobal) `shouldThrow` (== TestException)
