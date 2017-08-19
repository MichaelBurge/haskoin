{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Haskoin.Cli.Mine where

import Haskoin.Mining
import Haskoin.Serialization
import Haskoin.Types

import Protolude
import System.Environment
import Data.Binary
import qualified Data.ByteString.Lazy as BSL
import System.Directory
import Prelude(read)
import Data.Csv

defaultChainFile = "main.chain"
defaultAccount = "10"

main :: IO ()
main = do
  args <- getArgs
  let (filename, accountS) = case args of
        [] -> (defaultChainFile, defaultAccount)
        [filename] -> (filename, defaultAccount)
        [filename, account] -> (filename, account)
        _ -> panic "Usage: mine [filename] [account]"
      swapFile = filename ++ ".tmp"
      txnPool = return []
      account = Account $ read accountS
  forever $ do
    chain <- loadOrCreate filename makeGenesis :: IO Blockchain
    newChain <- mineOn txnPool account chain
    encodeFile swapFile newChain
    copyFile swapFile filename
    print "Block mined and saved!"

loadOrCreate :: Binary a => FilePath -> (IO a) -> IO a
loadOrCreate filename init = do
  exists <- doesFileExist filename
  if exists
    then decodeFile filename
    else do
      x <- init
      encodeFile filename x
      return x
