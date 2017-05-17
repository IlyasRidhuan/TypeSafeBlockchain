{-# LANGUAGE OverloadedStrings #-}

module AWS.DynamoDBHandler where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Data.ByteString         (ByteString)
import           Data.Conduit
import qualified Data.Conduit.List       as CL
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           Network.AWS.Data
import           Network.AWS.DynamoDB
import           Network.AWS.Auth
import           System.IO
import           Network.AWS.Data.List1
import qualified Data.List.NonEmpty   as NonEmpty

printTables :: Region -> IO ()
printTables region = do
    lgr <- newLogger Debug stdout
    env <- newEnv $ FromKeys "AKIAJ5Q7GQRONGS6VGEQ" "nXhTOVeS9qsWDQ9MnecXq8qn6EhoMRvYPgVliMxe"

    runResourceT . runAWST env . within region $ do
        say $ "Listing all tables in region " <> toText region
        paginate listTables
            =$= CL.concatMap (view ltrsTableNames)
             $$ CL.mapM_ (say . mappend "Table: ")

insertItem :: Region -> Text -> HashMap Text AttributeValue -> IO PutItemResponse
insertItem region table item = do
    lgr <- newLogger Debug stdout
    env <- newEnv $ FromKeys "AKIAJ5Q7GQRONGS6VGEQ" "nXhTOVeS9qsWDQ9MnecXq8qn6EhoMRvYPgVliMxe"

    runResourceT . runAWST env . within region $ do
        say $ "Inserting item into table '"
           <> table
           <> "' with attribute names: "
           <> Text.intercalate ", " (Map.keys item)
        send $ putItem table & piItem .~ item

batchInsert :: Region -> [Text] -> [HashMap Text AttributeValue] -> IO BatchWriteItemResponse
batchInsert region tables items = do
    lgr <- newLogger Debug stdout
    env <- newEnv $ FromKeys "AKIAJ5Q7GQRONGS6VGEQ" "nXhTOVeS9qsWDQ9MnecXq8qn6EhoMRvYPgVliMxe"
    let putItems = (\x -> putRequest & prItem .~ x) <$> items
    let wReq     = (\x -> [writeRequest  & wrPutRequest .~ Just x]) <$> putItems
    let batchReqItem = foldr ( uncurry Map.insert) Map.empty $ zip tables (NonEmpty.fromList <$> wReq)


    runResourceT . runAWST env . within region $ do
        say $ "Batch inserting"
        send $ batchWriteItem & bwiRequestItems .~ batchReqItem


getEntry :: Region -> Text -> HashMap Text AttributeValue -> IO GetItemResponse
getEntry region table item = do
    lgr <- newLogger Debug stdout
    env <- newEnv $ FromKeys "AKIAJ5Q7GQRONGS6VGEQ" "nXhTOVeS9qsWDQ9MnecXq8qn6EhoMRvYPgVliMxe"

    runResourceT . runAWST env . within region $ do
        say $ "Retrieving item from table '"
            <> table
            <> "' with keys: "
            <> Text.intercalate ", " (Map.keys item)
        send $ getItem table & giKey .~ item

updateEntry :: Region -> Text -> HashMap Text AttributeValue -> Text -> HashMap Text AttributeValue -> IO UpdateItemResponse
updateEntry region table key expr val = do
    lgr <- newLogger Debug stdout
    env <- newEnv $ FromKeys "AKIAJ5Q7GQRONGS6VGEQ" "nXhTOVeS9qsWDQ9MnecXq8qn6EhoMRvYPgVliMxe"

    runResourceT . runAWST env . within region $ do
        say "Updating "
        send $ updateItem table & uiKey .~ key & uiUpdateExpression .~ Just expr & uiExpressionAttributeValues .~ val

scanTable :: Region -> Text -> Text -> IO ScanResponse
scanTable region table scanFilter = do
    lgr <- newLogger Debug stdout
    env <- newEnv $ FromKeys "AKIAJ5Q7GQRONGS6VGEQ" "nXhTOVeS9qsWDQ9MnecXq8qn6EhoMRvYPgVliMxe"

    runResourceT . runAWST env . within region $ do
        say $ "Scanning items in table "
        send $ scan table & sFilterExpression .~ Just scanFilter



say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLn
