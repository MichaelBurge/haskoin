module Haskoin.Mining where

import Haskoin.Types

import Control.Comonad.Cofree
import Crypto.Hash
import Data.Binary
import Haskoin.Serialization
import qualified Data.Vector as V

type TransactionPool = IO [Transaction]

mineOn :: TransactionPool -> Account -> Blockchain -> IO Blockchain
mineOn pendingTransactions minerAccount parent = do
  ts <- pendingTransactions
  let block = Block (V.fromList ts)
  let header = BlockHeader {
        _miner = minerAccount,
        _parentHash = hashlazy $ encode parent
        }
  return $ block :< Node header parent

makeGenesis :: IO Blockchain
makeGenesis = return $ Block (V.fromList []) :< Genesis

testMining :: IO Blockchain
testMining = do
  let txnPool = return []
  chain <- makeGenesis
  chain <- mineOn txnPool 0 chain
  chain <- mineOn txnPool 0 chain
  chain <- mineOn txnPool 0 chain
  chain <- mineOn txnPool 0 chain
  chain <- mineOn txnPool 0 chain
  return chain
