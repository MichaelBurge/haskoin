{-# LANGUAGE NoImplicitPrelude #-}

module Haskoin.Mining where

import Haskoin.Types

import Control.Comonad.Cofree
import Crypto.Hash
import Crypto.Number.Serialize(os2ip)
import Data.Binary
import Haskoin.Serialization
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.Vector as V
import qualified Data.Map as M

import Protolude

type TransactionPool = IO [Transaction]

globalTransactionLimit = 1000
numBlocksToCalculateDifficulty = 5
genesisBlockDifficulty = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
targetTime = 10
blockReward = 1000

mineOn :: TransactionPool -> Account -> Blockchain -> IO Blockchain
mineOn pendingTransactions minerAccount parent = do
  ts <- pendingTransactions
  ts <- return $ validTransactions parent ts
  ts <- return $ take globalTransactionLimit ts
  now <- getPOSIXTime
  result <- loop now ts 0 (difficulty parent * 4)
  after <- getPOSIXTime
  print $ after - now
  return result
  where
    validChain bc = difficulty bc < desiredDifficulty parent
    loop now ts nonce bestDifficulty = do
      let header = BlockHeader {
            _miner = minerAccount,
            _parentHash = hashlazy $ encode parent,
            _nonce = nonce,
            _minedAt = now
            }
          block = Block (V.fromList ts)
          candidate = addBlock block header parent
          log = if candidateDifficulty < bestDifficulty
                then do
                  print ("New candidate found: ", logBase 10 $ fromIntegral $ desiredDifficulty parent, logBase 10 $ fromIntegral $ candidateDifficulty)
                  return candidateDifficulty
                else return bestDifficulty
                where
                  candidateDifficulty = difficulty candidate
      if validChain candidate
        then return candidate
        else log >>= loop now ts (nonce+1)

difficulty :: Blockchain -> Integer
difficulty bc = os2ip $ (hashlazy $ encode bc :: HaskoinHash)

-- BEWARE: O(n * k), where k = numBlocksToCalculateDifficulty
desiredDifficulty :: Blockchain -> Integer
desiredDifficulty x = round $ loop x
  where
    loop (_ :< Genesis) = genesisBlockDifficulty
    loop x@(_ :< Node _ xs) = oldDifficulty / adjustmentFactor
      where
        oldDifficulty = loop xs
        adjustmentFactor = min 4.0 $ targetTime `safeDiv` blockTimeAverage x

blockTimeAverage :: Blockchain -> NominalDiffTime
blockTimeAverage bc = average $ zipWith (-) times (fromMaybe [] $ tailMay times)
  where
    times = take numBlocksToCalculateDifficulty $ map _minedAt $ headers bc

chains :: Blockchain -> [Blockchain]
chains x@(_ :< Genesis) = [x]
chains x@(_ :< Node _ next) = x : chains next

headers :: Blockchain -> [BlockHeader]
headers (_ :< Genesis) = []
headers (_ :< Node x next) = x : headers next

average :: (Foldable f, Num a, Fractional a, Eq a) => f a -> a
average xs = sum xs `safeDiv` fromIntegral (length xs)

safeDiv n d = n / (if d == 0 then 1 else d)
                                                      
makeGenesis :: IO Blockchain
makeGenesis = return $ Block (V.fromList []) :< Genesis

balances :: Blockchain -> M.Map Account Integer
balances bc =
  let txns = toList $ mconcat $ toList bc
      debits = map (\Transaction{ _from = acc, _amount = amount} -> (acc, -amount)) txns
      credits = map (\Transaction{ _to = acc, _amount = amount} -> (acc, amount)) txns
      minings = map (\h -> (_miner h, blockReward)) $ headers bc
  in M.fromListWith (+) $ debits ++ credits ++ minings

validTransactions :: Blockchain -> [Transaction] -> [Transaction]
validTransactions bc txns =
  let accounts = balances bc
      validTxn txn = case M.lookup (_from txn) accounts of
        Nothing -> False
        Just balance -> balance >= _amount txn
  in filter validTxn txns

validateChain :: Blockchain -> Bool
validateChain _ = True

validateTxn :: Blockchain -> Transaction -> Bool
validateTxn _ _ = True

addBlock :: Block -> BlockHeader -> Blockchain -> Blockchain
addBlock block header chain = block :< Node header chain

----

testMining :: IO Blockchain
testMining = do
  let txnPool = return []
  chain <- makeGenesis
  chain <- mineOn txnPool 0 chain
  chain <- mineOn txnPool 0 chain
  chain <- mineOn txnPool 0 chain
  chain <- mineOn txnPool 0 chain
  chain <- mineOn txnPool 0 chain
  chain <- mineOn txnPool 0 chain
  chain <- mineOn txnPool 0 chain
  chain <- mineOn txnPool 0 chain
  chain <- mineOn txnPool 0 chain
  chain <- mineOn txnPool 0 chain
  -- chain <- mineOn txnPool 0 chain
  -- chain <- mineOn txnPool 0 chain
  -- chain <- mineOn txnPool 0 chain
  -- chain <- mineOn txnPool 0 chain
  -- chain <- mineOn txnPool 0 chain
  
  return chain
