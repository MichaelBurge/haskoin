{-# LANGUAGE GeneralizedNewtypeDeriving, NoImplicitPrelude, DeriveTraversable, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, DeriveGeneric #-}

module Haskoin.Types where

import Protolude
import Crypto.Hash

import Control.Comonad.Cofree
import Data.Data
import Data.Time.Clock.POSIX
import qualified Data.Vector as V

newtype Account = Account Integer deriving (Eq, Show, Num, Ord)

data Transaction = Transaction {
  _from   :: Account,
  _to     :: Account,
  _amount :: Integer
  } deriving (Eq, Show)

newtype BlockF a = Block (V.Vector a) deriving (Eq, Show, Foldable, Traversable, Functor, Monoid)
type Block = BlockF Transaction

type HaskoinHash = Digest SHA1

data BlockHeader = BlockHeader {
  _miner       :: Account,
  _parentHash  :: HaskoinHash,
  _nonce       :: Integer,
  _minedAt     :: POSIXTime
  } deriving (Eq, Show)

data MerkleF a = Genesis
               | Node BlockHeader a
               deriving (Eq, Show, Functor, Traversable, Foldable)

type Blockchain = Cofree MerkleF Block

deriving instance Generic Blockchain
-- deriving instance (Ger a, Data (f (Cofree f a)), Typeable f) => Data (Cofree f a)
