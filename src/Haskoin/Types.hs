{-# LANGUAGE GeneralizedNewtypeDeriving, NoImplicitPrelude, DeriveTraversable, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, DeriveGeneric #-}

module Haskoin.Types where

import Protolude
import Crypto.Hash

import Control.Comonad.Cofree
import Data.Data
import Data.Time.Clock.POSIX
import qualified Data.Vector as V
import Control.Concurrent.STM.TVar
import Network
import qualified Data.Set as S

newtype Account = Account Integer deriving (Eq, Show, Num, Ord)

data Transaction = Transaction {
  _from   :: Account,
  _to     :: Account,
  _amount :: Integer
  } deriving (Eq, Show, Ord)

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

data ServerState = ServerState {
  _longestChain :: TVar Blockchain,
  _supernodes   :: TVar (S.Set Supernode),
  _txnPool      :: TVar (S.Set Transaction)
  }

newtype SeedServer = SeedServer Text deriving (Eq, Show, Generic)
data Supernode = Supernode {
  _nodeName        :: Text,
  _nodeHost        :: HostName
  } deriving (Eq, Show, Generic, Ord)

data HaskoinRequest = ReqEntireBlockchain
                    | ReqNewBlock Block BlockHeader
                    | ReqNewTransaction Transaction
                    | ReqRegisterSupernode Supernode
                    | ReqListSupernodes
                    | ReqListTransactions
                    deriving (Eq, Show, Generic)

data HaskoinResponse = ResEntireBlockchain Blockchain
                     | ResNewBlock
                     | ResNewTransaction
                     | ResRegisterSupernode
                     | ResListSupernodes [Supernode]
                     | ResListTransactions [Transaction]
                     deriving (Eq, Show, Generic)
