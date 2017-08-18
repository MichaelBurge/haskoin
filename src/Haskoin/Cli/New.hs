{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Haskoin.Cli.New where

import Haskoin.Mining
import Haskoin.Serialization
import Haskoin.Types

import Protolude
import System.Environment
import Data.Binary
import qualified Data.ByteString.Lazy as BSL

defaultChainFile = "main.chain"

main :: IO ()
main = do
  args <- getArgs
  let filename = case args of
        [] -> defaultChainFile
        [x] -> x
        _ -> panic "Usage: new [filename]"
  chain <- makeGenesis
  BSL.writeFile filename $ encode chain
