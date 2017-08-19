{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Haskoin.Serialization where

import Haskoin.Types
import Control.Comonad.Cofree
import Crypto.Hash
import Data.Binary
import Data.Binary.Get
import Data.ByteArray
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Vector.Binary
import GHC.Generics

instance (Binary (f (Cofree f a)), Binary a) => Binary (Cofree f a) where
instance (Binary a) => Binary (MerkleF a) where
instance Binary BlockHeader where
instance Binary Transaction where
instance Binary POSIXTime where
  get = fromInteger <$> (get :: Get Integer)
  put x = put $ (round x :: Integer)

instance Binary SeedServer where
instance Binary Supernode where
instance Binary HaskoinRequest where
instance Binary HaskoinResponse where
deriving instance Binary Account
deriving instance Binary Block

deriving instance Generic (Cofree f a)
deriving instance Generic (MerkleF a)
deriving instance Generic BlockHeader
deriving instance Generic Transaction
instance Binary HaskoinHash where
  get = do
    mDigest <- digestFromByteString <$> (get :: Get BS.ByteString)
    case mDigest of
      Nothing -> fail "Not a valid digest"
      Just digest -> return digest
  put digest = put $ (convert digest :: BS.ByteString)
