{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Blockchain.BraidedBlockchain where

import Crypto.Hash
import Data.List (zipWith5,maximumBy)
import Blockchain.GenAddress
import Blockchain.Patricia
import Control.Monad
import Control.Applicative
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
import Data.Ord (comparing)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict,fromStrict)
import           Data.HashMap.Strict     (HashMap,insert, (!),lookup)
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

-- getCIDTree :: (Binary k, Binary v) => Hash -> Tx -> MaybeT IO (PatriciaTree Text Hash)
-- getCIDTree hash Tx {..} = do
--     let treeRoot = Map.singleton "Hash" (attributeValue & avS .~ (Just $ Data.Text.pack $ show hash))
--     res <- liftIO $ getEntry NorthVirginia "MusicStateDB" treeRoot
--     serialBinaryTree <- (MaybeT . return) $ ((res ^. girsItem) ! "Content") ^. avB
--     return $ decode $ fromStrict serialBinaryTree
--
getBlockchainTree:: Bead -> MaybeT IO (PatriciaTree Char Char)
getBlockchainTree Bead {..} = do
    let treeRoot = Map.singleton "Hash" (attributeValue & avS .~ (Just $ Data.Text.pack $ show patriciaRoot))
    res <- liftIO $ getEntry NorthVirginia "ChainData" treeRoot
    serialBinaryTree <- (MaybeT . return) $ ((res ^. girsItem) ! "TreeData") ^. avB
    return $ decode $ fromStrict serialBinaryTree

getChainTip :: MaybeT IO Bead
getChainTip = do
    blocks <- liftIO $ scanTable NorthVirginia "BlockChain" "attribute_exists(BlockData)"
    blockNumber <- (MaybeT . return ) $ traverse (\x -> x ! "BlockNumber" ^. avS) (blocks ^. srsItems)
    blockData <- (MaybeT . return ) $ traverse (\x -> x ! "BlockData" ^. avB) (blocks ^. srsItems)
    let listedVersion = zip blockNumber blockData
    let (num,tipData) = maximumBy (comparing fst) listedVersion
    return $ decode $ fromStrict tipData


seedBlockchain :: IO PutItemResponse
seedBlockchain = do
    let genesisBlock = toStrict $ encode genesisBead
    let zerothBlock = insert "BlockNumber" (attributeValue & avS .~ Just "0") $ insert "BlockData" (attributeValue & avB .~ Just genesisBlock) Map.empty
    insertItem NorthVirginia "BlockChain" zerothBlock

seedChainData :: MaybeT IO PutItemResponse
seedChainData = do
    entries <- liftIO $ scanTable NorthVirginia "StateData" "attribute_exists(TreeData)"
    cidList <- (MaybeT . return ) $ traverse (\x -> x ! "ContractID" ^. avS) (entries ^. srsItems)
    entriesList <- (MaybeT . return ) $ traverse (\x -> x ! "TreeData" ^. avB) (entries ^. srsItems)
    roots <- (MaybeT . return ) $ traverse
        (getRoot . (decode . fromStrict :: ByteString -> PatriciaTree Char Char)) entriesList
    let cidHashTree = foldr (\x ->  flip ( `insertPatriciaTree` fst x) (snd x)) singleton $ zip (Data.Text.unpack <$> cidList) (show <$> roots)
    let encCHTree = toStrict $ encode cidHashTree
    chTreeRoot <- (MaybeT . return ) $ getRoot cidHashTree
    let seedTree = insert "Hash" (attributeValue & avS .~ Just (Data.Text.pack $ show chTreeRoot)) $ insert "TreeData" (attributeValue & avB .~ Just encCHTree) Map.empty
    liftIO $ insertItem NorthVirginia "ChainData" seedTree

    -- (MaybeT . return ) $ traverse getRoot trees
    -- return $ foldr (\x -> flip ( `insertPatriciaTree` fst x) (snd x)) singleton entriesList

seedStateData :: MaybeT IO PutItemResponse
seedStateData =  do
    entries <- liftIO $ scanTable NorthVirginia "Music" "attribute_exists(Address)"
    entriesList <-  (MaybeT . return ) $ listify (entries ^. srsItems)
    let addrHashTree = foldr (\x -> flip ( `insertPatriciaTree` fst x) (snd x)) singleton entriesList
    let encodedTree = toStrict $ encode addrHashTree
    root <- (MaybeT . return ) $ getRoot addrHashTree
    let seedTree = insert "Hash" (attributeValue & avS .~ Just (Data.Text.pack $ show root)) $ insert "ContractID" (attributeValue & avS .~ Just "Music") $ insert "TreeData" (attributeValue & avB .~ Just encodedTree) Map.empty
    liftIO $ insertItem NorthVirginia "StateData" seedTree

listify :: [HashMap Text AttributeValue]  -> Maybe [(String,String)]
listify = traverse (\x -> do
    maybeAddress <- Data.Text.unpack <$> (Data.HashMap.Strict.lookup "Address" x >>= flip (^.) avS)
    maybeHash <- Data.Text.unpack <$> (Data.HashMap.Strict.lookup "Artist" x >>= flip (^.) avS)
    hash <- (digestFromByteString .fst .B16.decode . B8.pack) maybeHash :: Maybe Hash
    return (maybeAddress, show hash))
