{-# LANGUAGE GeneralizedNewtypeDeriving, NoImplicitPrelude, DeriveTraversable, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}

module Haskoin.Types where

import Protolude
import Crypto.Hash

import Control.Comonad.Cofree
import Data.Data
import qualified Data.Vector as V

newtype Account = Account Integer deriving (Eq, Show, Num)

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
  _parentHash  :: HaskoinHash
  } deriving (Eq, Show)

data MerkleF a = Genesis
               | Node BlockHeader a
               deriving (Eq, Show, Functor, Traversable, Foldable)

type Blockchain = Cofree MerkleF Block
