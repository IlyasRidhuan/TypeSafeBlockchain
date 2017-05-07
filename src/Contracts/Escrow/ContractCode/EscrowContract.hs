{-# LANGUAGE TypeOperators,UndecidableInstances,DeriveFunctor,StandaloneDeriving, GADTs, FlexibleContexts, OverloadedStrings, TemplateHaskell,DeriveGeneric #-}
module Contracts.Escrow.ContractCode.EscrowContract where

import Contracts.Escrow.ContractCode.TransferLib
import Contracts.Escrow.ContractCode.StorageLib
import Contracts.Escrow.ContractCode.FreeLib
import Data.Functor.Identity
import Data.Map
import Control.Concurrent.STM
import Prelude hiding (lookup)
import Control.Monad.Trans.Free
import Control.Monad.IO.Class
import Control.Lens
import           GHC.Generics hiding ((:+:))

type DudEscrowF = StorageF :+: TransferOpF
type DudEscrowT = FreeT DudEscrowF

data Escrow = Escrow {
    escrowAddr :: String
    ,ownerAddr :: String
    -- ,currAmount :: Address
    -- ,maxAmount :: Address
    ,currAmount :: Amount
    ,maxAmount :: Amount
} deriving (Generic,Show)

makeLenses ''Escrow

createEscrow :: (MonadFree f m, StorageF :<: f, TransferOpF :<: f) => String -> String -> Double -> m Escrow
createEscrow escrowAddr ownerAddr amt = do
    currAmountAcct <- createAddr 0
    maxAmountAcct  <- createAddr amt
    return $ Escrow escrowAddr ownerAddr 0 amt


-- sendMonies :: (MonadIO m, MonadFree f m, TransferOpF :<: f, StorageF :<: f) => Address -> Address -> Escrow -> Amount -> m (Escrow,Address,Address)
-- sendMonies senderAcct ownerAcct e amt = do
--     transfer senderAcct (currAmount e) amt
--     escrow_balance <- getBalance (currAmount e)
--     limit <- getBalance (maxAmount e)
--
--     if escrow_balance >= limit
--         then do
--             owner_balance <- getBalance ownerAcct
--             transfer (currAmount e) ownerAcct limit
--             escrow_balance' <- getBalance (currAmount e)
--             transfer (currAmount e) senderAcct escrow_balance'
--             newAmountAcct <- createAddr 0
--             return (Escrow (escrowAddr e) (ownerAddr e) (currAmount e) newAmountAcct,senderAcct,ownerAcct)
--
--         else return (e,senderAcct,ownerAcct)

sendMonies :: (MonadIO m, MonadFree f m, TransferOpF :<: f, StorageF :<: f) => Amount -> Amount -> Escrow -> Amount -> m (Escrow,Amount,Amount)
sendMonies sender owner e amt = do
    senderAcct <- createAddr sender
    ownerAcct <- createAddr owner
    escCurrAcct <- createAddr (currAmount e)
    escMaxAcct  <- createAddr (maxAmount e)
    transfer senderAcct escCurrAcct amt
    escrow_balance <- getBalance escCurrAcct
    limit <- getBalance escMaxAcct

    if escrow_balance >= limit
        then do
            owner_balance <- getBalance ownerAcct
            transfer escCurrAcct ownerAcct limit
            escrow_balance' <- getBalance escCurrAcct
            transfer escCurrAcct senderAcct escrow_balance'
            newCurrBal <- getBalance escCurrAcct
            senderBal <- getBalance senderAcct
            ownerBal  <- getBalance ownerAcct
            return (Escrow (escrowAddr e) (ownerAddr e) newCurrBal 0,senderBal,ownerBal)

        else do
            newCurrBal' <- getBalance escCurrAcct
            senderBal' <- getBalance senderAcct
            ownerBal'  <- getBalance ownerAcct
            return (Escrow (escrowAddr e) (ownerAddr e) newCurrBal' (maxAmount e),senderBal',ownerBal')

dudI :: DudEscrowT IO r -> IO r
dudI a = do
    mr <- runFreeT a
    case mr of
        Pure r -> return r
        Free (Inl x) -> do
            d <- storageI (liftF x)
            dudI d
        Free (Inr y) -> do
            c <- atomically $ transferOpI (liftF y)
            dudI c


-- runThis :: IO ()
-- runThis = dudI $ do
--     -- liftIO $ putStrLn "HEllo"
--     -- m <- createStore
--     -- test <- createAddr 1000
--     -- m'<- insertElement "test" test m
--     m1 <- createEscrow 500 "owner"
--     sendMonies m1 "test1" 750
--     return ()
