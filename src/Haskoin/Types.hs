{-# LANGUAGE NoImplicitPrelude, DeriveTraversable, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}

module Haskoin.Types where

import Protolude
import Crypto.Hash

import Control.Comonad.Cofree
import Data.Data
import qualified Data.Vector as V

newtype Account = Account Integer deriving (Eq, Show)

data Transaction = Transaction {
  _from   :: Account,
  _to     :: Account,
  _amount :: Integer
  } deriving (Eq, Show)

newtype Block = Block (V.Vector Transaction) deriving (Eq, Show)
type HaskoinHash = Digest SHA1

data MerkleF a = Genesis
               | Node HaskoinHash [a]
               deriving (Eq, Show, Functor, Traversable, Foldable)

type Blockchain = Cofree MerkleF Block
