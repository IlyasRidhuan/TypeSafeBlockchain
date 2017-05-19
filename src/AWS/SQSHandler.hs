{-# LANGUAGE OverloadedStrings #-}
module AWS.SQSHandler where

import           Data.Text               (Text,pack)
import qualified Data.Text.IO            as Text

import           System.IO
import Network.AWS.SQS
import           Network.AWS.Data
import           Network.AWS.Auth
import           Control.Monad.Trans.AWS
import Data.HashMap.Strict
import Control.Lens
import           Data.Monoid
import           Control.Monad.IO.Class

getMessage :: Region -> Text -> [Text] -> IO ReceiveMessageResponse
getMessage region qUrl msgAttribNames = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr

    runResourceT . runAWST env . within region $ do
        say $ "Receiving Message from queue: " <> qUrl
        send $ receiveMessage qUrl & rmMessageAttributeNames .~ msgAttribNames & rmWaitTimeSeconds ?~ 20 & rmMaxNumberOfMessages ?~ 10

delMessage :: Region -> Text -> Text -> IO DeleteMessageResponse
delMessage region qUrl receiptHandle = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr

    runResourceT . runAWST env . within region $ do
        say $ "Deleting Message from queue: " <> qUrl
        send $ deleteMessage qUrl receiptHandle

batchDelMessage :: Region -> Text -> [Text] -> IO DeleteMessageBatchResponse
batchDelMessage region qUrl receiptHandles = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr

    let pid = (Data.Text.pack . show ) <$> [1..length receiptHandles]
    let deleteHandles = uncurry deleteMessageBatchRequestEntry <$> zip receiptHandles pid
    runResourceT . runAWST env . within region $ do
        say $ "Batching delete message from queue: " <> qUrl
        send $ deleteMessageBatch qUrl & dmbEntries .~ deleteHandles

sndMessage :: Region -> Text -> [Text] -> IO [SendMessageResponse]
sndMessage region qUrl xs = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr
    
    runResourceT . runAWST env . within region $ do
        say $ "Sending message to queue: " <> qUrl
        traverse (send . sendMessage qUrl ) xs

say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLn
