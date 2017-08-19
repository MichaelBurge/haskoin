{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Haskoin.Cli.Server where

import Haskoin.Network
import Protolude

main :: IO ()
main = runSeedServer
