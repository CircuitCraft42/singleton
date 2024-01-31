module Unique (Unique, newUnique) where

import Control.Singleton
import Data.IORef

newtype Unique = Unique Integer deriving (Eq, Ord)

newtype UniqueSource = UniqueSource (IORef Integer)

instance Singleton UniqueSource where
  initialize = UniqueSource <$> newIORef 0

newUnique :: IO Unique
newUnique = do
  UniqueSource source <- only
  Unique <$> atomicModifyIORef' source (\x -> let z = x + 1 in (z, z))
