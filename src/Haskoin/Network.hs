{-# LANGUAGE NoImplicitPrelude, RecordWildCards, LambdaCase, ViewPatterns, OverloadedStrings #-}

module Haskoin.Network where

import Haskoin.Types
import Haskoin.Serialization
import Haskoin.Mining

import Data.Binary
import qualified Network.Wai as Warp
import Network.Wai.Handler.Warp as Warp
import Network.HTTP.Simple
import Network.HTTP.Client
import Network.HTTP.Types
import Network.HTTP.Types.Method
import qualified Data.ByteString as BSL
import qualified Data.Set as S
import qualified Data.Text as T
import Protolude
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import System.Directory

chainFile = "main.chain"
supernodesFile = "supernodes.bin"
seedServer = "haskoin.michaelburge.us"
haskoinPort = 31337

seedNode = Supernode "Seed" "haskoin.michaelburge.us"

runSeedServer :: IO ()
runSeedServer = run haskoinPort =<< appBase <$> initSeedServer
  
-- runMiningServer :: Account -> IO ()
-- runMiningServer account = do
--   state <- initMiner
--   loop = do
--     minerPid <- forkIO $ do
--       chain <- readTVarIO $ _longestChain state
--       txns <- readTVarIO $ _txnPool state
--       (block :< Node header _) <- mineOn txns account chain
--       sendReq seedNode $ ReqNewBlock block header
--       return ()
  
--   appBase state
--   where
--     seconds = 1000000

appBase :: ServerState -> Warp.Application
appBase state = \req respond -> do
  reqBs <- Warp.strictRequestBody req
  let haskReq = decode reqBs
  haskRes <- appropriateResponse state haskReq
  respond $ Warp.responseLBS status200 [] (encode haskRes)

initSeedServer :: IO ServerState
initSeedServer = do
  chain      <- loadOrCreate chainFile makeGenesis
  supernodes <- loadOrCreate supernodesFile (return [seedNode])
  ServerState <$> newTVarIO chain <*> newTVarIO S.empty <*> newTVarIO S.empty

initMiner :: IO ServerState
initMiner = do
  (ResEntireBlockchain chain)    <- sendReq seedNode ReqEntireBlockchain
  (ResListSupernodes supernodes) <- sendReq seedNode ReqListSupernodes
  (ResListTransactions txns)     <- sendReq seedNode ReqListTransactions
  ServerState <$> newTVarIO chain <*> newTVarIO (S.fromList supernodes) <*> newTVarIO (S.fromList txns)
  
loadOrCreate :: Binary a => FilePath -> (IO a) -> IO a
loadOrCreate filename init = do
  exists <- doesFileExist filename
  if exists
    then decodeFile filename
    else do
      x <- init
      encodeFile filename x
      return x

sendReq :: Supernode -> HaskoinRequest -> IO HaskoinResponse
sendReq Supernode{..} haskReq = do
  let request = defaultRequest {
        method = "POST",
        host = encodeUtf8 $ T.pack _nodeHost,
        port = haskoinPort,
        requestBody = RequestBodyLBS (encode haskReq)
        }
  resBs <- httpLBS request
  return $ decode $ getResponseBody resBs
  
appropriateResponse :: ServerState -> HaskoinRequest -> IO HaskoinResponse
appropriateResponse ServerState{..} = \case
  ReqEntireBlockchain -> ResEntireBlockchain <$> (readTVarIO _longestChain)
  ReqNewBlock block header -> atomically $ do
    always $ validateChain <$> readTVar _longestChain
    modifyTVar' _longestChain (addBlock block header) `orElse` return ()
    return ResNewBlock
  ReqNewTransaction txn -> atomically $ do
    always $ validateTxn <$> readTVar _longestChain <*> return txn
    modifyTVar' _txnPool (S.insert txn) `orElse` return ()
    return ResNewTransaction
  ReqRegisterSupernode supernode -> atomically $ do
    modifyTVar' _supernodes (S.insert supernode)
    return ResRegisterSupernode
  ReqListSupernodes -> ResListSupernodes <$> S.toList <$> readTVarIO _supernodes
  ReqListTransactions -> ResListTransactions <$> S.toList <$> readTVarIO _txnPool
