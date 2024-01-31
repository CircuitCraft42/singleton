{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Singleton (Singleton (initialize), only) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Dynamic
import Data.Proxy
import qualified Focus as F
import qualified StmContainers.Map as M
import System.IO.Unsafe
import Type.Reflection

class (Typeable a) => Singleton a where
  initialize :: IO a

data Instance = InProgress | Errored SomeException | Complete Dynamic

singletonDict :: M.Map SomeTypeRep Instance
{-# NOINLINE singletonDict #-}
singletonDict = unsafePerformIO M.newIO

only :: forall a. (Singleton a) => IO a
only = mask $ \restore -> do
  status <-
    atomically . join $
      M.focus
        ( F.lookup >>= \case
            Nothing -> F.insert InProgress >> pure (pure InProgress)
            Just InProgress -> pure retry
            Just (Errored e) -> pure (pure (Errored e))
            Just (Complete x) -> pure (pure (Complete x))
        )
        someType
        singletonDict
  case status of
    Complete x -> pure (fromDyn x (error "Ill-typed entry in Singleton lookup table"))
    Errored e -> throwIO e
    InProgress -> do
      result <- try (restore initialize)
      case result of
        Right val -> do
          atomically $ M.insert (Complete $ toDyn val) someType singletonDict
          pure val
        Left e -> do
          atomically $ M.insert (Errored e) someType singletonDict
          throwIO e
  where
    someType = someTypeRep (Proxy :: Proxy a)
