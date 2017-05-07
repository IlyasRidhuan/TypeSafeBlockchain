{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Contracts.Escrow.ContractCode.EscrowAPI where

import           Data.Aeson
import           Data.Aeson.Types
import           Servant
import           Crypto.Hash
import           GHC.Generics

import           Data.ByteString hiding (map)
import qualified Data.ByteString.Char8 as BC
-- import qualified Data.ByteString.Lazy as BL
import Control.Monad.Trans
import Data.Monoid
import           Database.Redis hiding (decode)
import           Data.ByteString.Lazy (toStrict)

import           Data.Maybe
import Control.Monad.Trans.Maybe
import           Data.Either
import Data.Traversable (traverse)

import Contracts.Escrow.ContractCode.EscrowContract
import Contracts.Escrow.ContractCode.TransferLib
import Control.Concurrent.STM.TVar

import AWS.DynamoDBHandler
import Network.AWS.Types
import           Network.AWS.Data
import           Network.AWS.DynamoDB
import           Network.AWS.Auth
import           System.IO

import           Data.HashMap.Strict     (HashMap,insert, (!))
import qualified Data.HashMap.Strict     as Map

import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import Data.Text.Encoding
import           Control.Lens            hiding ((.=),traverse)

import Network.Wai
import Network.Wai.Handler.Warp
import Data.Proxy
import System.IO.Unsafe

import Blockchain.GenAddress (genKey,AccountAddress,unAA)

data EscrowAPIPacket = EscrowAPIPacket {
    addr :: String
    ,content :: Content
} deriving (Show, Generic,FromJSON)

data Content = Esc Escrow | Amt Amount deriving (Show,Generic,FromJSON)

instance ToJSON Content where
    toJSON (Esc e) = object ["Escrow" .= e]

    toJSON (Amt amt) = object["Amount" .= amt]

instance ToJSON EscrowAPIPacket where
    toJSON (EscrowAPIPacket a c) = object ["addr" .= a, "content" .= c]

instance ToJSON Escrow where
    toJSON (Escrow ea oa ca ma) = object ["escrowAddr" .= ea, "ownerAddr" .= oa, "currAmount" .= ca, "maxAmount" .= ma]
instance FromJSON Escrow

type NodeAPI ="createEscrow" :> Capture "escrowAddr" String :> Capture "ownerAddr" String :> Capture "amount" String :> Get '[JSON] String
        :<|>  "getBalance" :> Capture "address" String :> Get '[JSON] EscrowAPIPacket
        :<|>  "payEscrow" :> Capture "escrowAddr" String :> Capture "senderAddr" String :> Capture "amount" String :> Get '[JSON] String
        :<|>  "createAccount" :> Capture "amount" String :> Get '[JSON] String
        :<|>  "get" :> Get '[JSON] [EscrowAPIPacket]

parseDynamoJSON' :: Escrow -> HashMap Text AttributeValue
parseDynamoJSON' Escrow {escrowAddr=ea,ownerAddr=oa,currAmount=camt,maxAmount=mamt} =
    insert "escrowAddr" (attributeValue & avS .~ Just (Text.pack ea)) $ insert "ownerAddr" (attributeValue & avS .~ Just (Text.pack oa)) $ insert "currAmount" (attributeValue & avN .~ Just (Text.pack $ show camt)) $ insert "maxAmount" (attributeValue & avN .~ Just (Text.pack $ show mamt)) Map.empty

mkEscrow :: Monad m => GetItemResponse -> MaybeT m Escrow
mkEscrow res = MaybeT .return $ do
    ea <- Text.unpack <$> (((res ^. girsItem) ! "Escrow") ^. avM) ! "escrowAddr" ^. avS
    oa <- Text.unpack <$> (((res ^. girsItem) ! "Escrow") ^. avM) ! "ownerAddr" ^. avS
    ca <- read . Text.unpack <$> (((res ^. girsItem) ! "Escrow") ^. avM) ! "currAmount" ^. avN
    ma <- read . Text.unpack <$> (((res ^. girsItem) ! "Escrow") ^. avM) ! "maxAmount" ^. avN
    return $ Escrow ea oa ca ma

mkAddr :: Monad m => GetItemResponse -> MaybeT m Amount
mkAddr res = MaybeT . return $
    read . Text.unpack <$> ((res ^. girsItem) ! "Amount" ^. avN)

mkEscrowAPIPacket :: Monad m => [HashMap Text AttributeValue] -> MaybeT m [EscrowAPIPacket]
mkEscrowAPIPacket = traverse (\x -> MaybeT . return $ do
    addr <- Text.unpack <$> (x ! "Artist") ^. avS
    ea <- Text.unpack <$> ((x ! "Escrow") ^. avM) ! "escrowAddr" ^. avS
    oa <- Text.unpack <$> ((x ! "Escrow") ^. avM) ! "ownerAddr" ^. avS
    ca <- read . Text.unpack <$> ((x ! "Escrow") ^. avM) ! "currAmount" ^. avN
    ma <- read . Text.unpack <$> ((x ! "Escrow") ^. avM) ! "maxAmount" ^. avN
    return $ EscrowAPIPacket addr (Esc $ Escrow ea oa ca ma))

mkAccountAPIPacket :: Monad m => [HashMap Text AttributeValue] -> MaybeT m [EscrowAPIPacket]
mkAccountAPIPacket = traverse (\x -> (MaybeT . return $ do
    addr <- Text.unpack <$> (x ! "Artist") ^. avS
    amt <- read . Text.unpack <$> ((x ! "Amount") ^. avN)
    return $ EscrowAPIPacket addr (Amt amt) ))


nodeAPIDelegator :: Server NodeAPI
nodeAPIDelegator = createEscrowHandler :<|> balEscrowHandler :<|> payEscrowHandler :<|> createAccountHandler :<|> getHandler

    where
        createEscrowHandler :: String -> String -> String -> Handler String
        createEscrowHandler ea oa amt = liftIO $ do
            e <- dudI $ createEscrow ea oa (read amt :: Double)

            let btstr = toStrict $ encode ea
            let atarr = parseDynamoJSON' e

            addrAcct <- genKey

            let avArr = insert "Address" (attributeValue & avS .~ (Just $ Text.pack $ BC.unpack (unAA addrAcct))) $ insert "Escrow" (attributeValue & avM .~ atarr) $ insert "Artist" (attributeValue & avS .~ (Just $ Text.pack $ show $ hashWith SHA256 (toStrict $ encode ea))) Map.empty

            res <- insertItem NorthVirginia "Music" avArr

            return $ show res            --

        payEscrowHandler :: String -> String -> String -> Handler String
        payEscrowHandler ea sa amt = maybeToExceptT err404 $ do
            let escKey = Map.singleton "Artist" (attributeValue & avS .~ (Just $ Text.pack $ show $ hashWith SHA256 $ toStrict $ encode ea))

            escrow <- liftIO $ getEntry NorthVirginia "Music" escKey
            escResult <- mkEscrow escrow

            let sendKey = Map.singleton "Artist" (attributeValue & avS .~ (Just $ Text.pack sa))

            sender <- liftIO $ getEntry NorthVirginia "Music" sendKey
            sendResult <- mkAddr sender
            liftIO $ print sendResult
            let ownKey = Map.singleton "Artist" (attributeValue & avS .~ (Just $ Text.pack (ownerAddr escResult)))

            owner <- liftIO $ getEntry NorthVirginia "Music" ownKey
            ownerResult <- mkAddr owner

            (e,senderRes,ownerRes) <- liftIO $ dudI $ sendMonies sendResult ownerResult escResult (read amt :: Double)
            liftIO $ print senderRes

            let atarr = parseDynamoJSON' e

            resEsc <- liftIO $ updateEntry NorthVirginia "Music" escKey "set Escrow = :esc" (Map.singleton ":esc" (attributeValue & avM .~ atarr))

            resSend <- liftIO $ updateEntry NorthVirginia "Music" sendKey "set Amount = :amt" (Map.singleton ":amt" (attributeValue & avN .~ (Just $ Text.pack $ show senderRes)))

            resOwn <- liftIO $ updateEntry NorthVirginia "Music" ownKey "set Amount = :amt" (Map.singleton ":amt" (attributeValue & avN .~ (Just $ Text.pack $ show ownerRes)))

            return $ show resEsc            --

        balEscrowHandler :: String -> Handler EscrowAPIPacket
        balEscrowHandler ea = maybeToExceptT err404 $ do
            let escKey = Map.singleton "Artist" (attributeValue & avS .~ (Just $ Text.pack $ show $ hashWith SHA256 $ toStrict $ encode ea))

            escrow <- liftIO $ getEntry NorthVirginia "Music" escKey
            esc <- mkEscrow escrow
            return $ EscrowAPIPacket (show $ hashWith SHA256 $ toStrict $ encode ea) (Esc esc)



        createAccountHandler :: String -> Handler String
        createAccountHandler amt = maybeToExceptT err404 $ do

            addrAcct <- liftIO genKey

            let addrEnt = insert "Address" (attributeValue & avS .~ (Just $ Text.pack $ BC.unpack (unAA addrAcct))) $ insert "Amount" (attributeValue & avN .~ (Just $ Text.pack amt)) $ insert "Artist" (attributeValue & avS .~ (Just $ Text.pack $ show $ hashWith SHA256 (unAA addrAcct <> BC.pack amt))) Map.empty

            res <- liftIO $ insertItem NorthVirginia "Music" addrEnt
            return $ show res

        getHandler :: Handler [EscrowAPIPacket]
        getHandler =  maybeToExceptT err404 $ do
            escrows <- liftIO $ scanTable NorthVirginia "Music" "attribute_exists(Escrow)"
            escs <- mkEscrowAPIPacket $ escrows ^. srsItems
            accounts <- liftIO $ scanTable NorthVirginia "Music" "attribute_exists(Amount)"
            accts <- mkAccountAPIPacket $ accounts ^. srsItems
            return $ escs <> accts

app :: Application
app = serve (Proxy :: Proxy NodeAPI) nodeAPIDelegator

main :: IO ()
main = run 3000 app
