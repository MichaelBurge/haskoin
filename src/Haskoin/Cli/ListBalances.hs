{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Haskoin.Cli.ListBalances where

import Haskoin.Mining
import Haskoin.Serialization
import Haskoin.Types

import Protolude
import System.Environment
import Data.Binary
import qualified Data.Map as M

defaultChainFile = "main.chain"

main :: IO ()
main = do
  args <- getArgs
  let (filename) = case args of
        [] -> (defaultChainFile)
        [filename] -> (filename)
        _ -> panic "Usage: list-balances [filename]"
  chain <- decodeFile filename :: IO Blockchain
  forM_ (M.toAscList $ balances chain) $ \(account, balance) -> do
    print (account, balance)
