{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Blockchain.BraidedBlockchain where

import Crypto.Hash
import Data.List
import Blockchain.GenAddress
import Blockchain.Patricia
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16
import Data.Binary (Binary, encode,decode)
import AWS.SQSHandler
import AWS.DynamoDBHandler
import Network.AWS.SQS
import Control.Monad.Trans.Maybe
import           Network.AWS.DynamoDB
import Control.Lens
import GHC.Generics
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict,fromStrict)
import           Data.HashMap.Strict     (HashMap,insert, (!))
import qualified Data.HashMap.Strict     as Map
import           Data.Text               (Text,unpack,pack)
import qualified Data.Text.IO            as Text
import Network.AWS.Types

data Tx = Tx {
    itemHash :: Hash,
    address :: AccountAddress,
    receipt :: TxReceipt,
    contractID :: Text,
    operation :: Text
} deriving (Show, Generic)


newtype TxReceipt = TxReceipt {unRec :: Text} deriving (Show)

type Mempool = [Tx]

data Bead = Bead {
    nonce :: Integer,
    timestamp :: Integer,
    patriciaRoot :: Hash,
    parents :: [Hash]
} deriving (Show,Generic)

instance Binary Bead

newtype Braid = Braid [Bead]

genesisBead :: Bead
genesisBead = Bead {nonce=0 ,timestamp = 0, patriciaRoot = genesisHash, parents = genesisParent}

genesisHash :: Hash
genesisHash = hash ("0" :: ByteString)

genesisParent :: [Hash]
genesisParent = [hash ("No Parent" :: ByteString)]

sampleNAddress :: Integer -> IO [AccountAddress]
sampleNAddress n = mapM (const genKey) [1..n]

-- sampleMempool :: Integer -> IO Mempool
-- sampleMempool n = do
--     aa <- sampleNAddress n
--     let hsh = hash <$> ((B8.pack . show ) <$> [1..n]) :: [Hash]
--     return $ zipWith (\itemHash address -> Tx {itemHash, address}) hsh aa

decodeMessage :: ReceiveMessageResponse -> Maybe [Tx]
decodeMessage res = do
    let messages = res ^. rmrsMessages
    receiptHandle <- traverse (^. mReceiptHandle ) messages
    addrs <- traverse (\x -> (x ^. mMessageAttributes) ! "Address" ^. mavStringValue  ) messages
    let validAddrs = (B8.pack . Data.Text.unpack ) <$> addrs
    hashes <- traverse (\x -> (x ^. mMessageAttributes) ! "Hash" ^. mavStringValue  ) messages
    validHashes <- traverse (digestFromByteString .fst .B16.decode . B8.pack . Data.Text.unpack) hashes
    cids <- traverse (\x -> (x ^. mMessageAttributes) ! "ContractID" ^. mavStringValue  ) messages
    -- validCIDs <- traverse (digestFromByteString .fst .B16.decode . B8.pack . Data.Text.unpack) cids

    op <- traverse (\x -> (x ^. mMessageAttributes) ! "Operation" ^. mavStringValue  ) messages
    -- maybe (deleteMessageResponse) (delMessage NorthVirginia "url here") (head receiptHandle)
    return $ zipWith5 (\a b c d e -> Tx a (AccountAddress b) (TxReceipt c) d e ) validHashes validAddrs receiptHandle cids op

getCIDTree :: Binary a => Hash -> Tx -> MaybeT IO (PatriciaTree a)
getCIDTree hash Tx {..} = do
    let treeRoot = Map.singleton "Hash" (attributeValue & avS .~ (Just $ Data.Text.pack $ show hash))
    res <- liftIO $ getEntry NorthVirginia "MusicStateDB" treeRoot
    serialBinaryTree <- (MaybeT . return) $ ((res ^. girsItem) ! "Content") ^. avB
    return $ decode $ fromStrict serialBinaryTree

getBlockchainTree:: Binary a => Hash -> Tx -> MaybeT IO (PatriciaTree a)
getBlockchainTree hash Tx {..} = do
    let treeRoot = Map.singleton "Hash" (attributeValue & avS .~ (Just $ Data.Text.pack $ show hash))
    res <- liftIO $ getEntry NorthVirginia "ChainData" treeRoot
    serialBinaryTree <- (MaybeT . return) $ ((res ^. girsItem) ! "Content") ^. avB
    return $ decode $ fromStrict serialBinaryTree

getChainTip :: MaybeT IO Bead
getChainTip = do
    let chainTip = Map.singleton "Hash" (attributeValue & avS .~ (Just $ Data.Text.pack "ChainTip"))
    res <- liftIO $ getEntry NorthVirginia "ChainData" chainTip
    tipNumber <- (MaybeT . return ) $ ((res ^. girsItem) ! "BlockNumber") ^. avS
    let getBlockTip = Map.singleton "Hash" (attributeValue & avS .~ Just tipNumber)
    blockRes <- liftIO $ getEntry NorthVirginia "ChainData" getBlockTip
    blockData <- (MaybeT . return ) $ ((res ^. girsItem) ! "BlockData") ^. avB
    return $ decode $ fromStrict blockData
