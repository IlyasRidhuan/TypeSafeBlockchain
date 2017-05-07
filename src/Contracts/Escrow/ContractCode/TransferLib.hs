{-# LANGUAGE DeriveFunctor, GADTs, StandaloneDeriving,TypeOperators,FlexibleContexts,MultiParamTypeClasses #-}
module Contracts.Escrow.ContractCode.TransferLib where

import Control.Monad.Trans.Free
import Control.Concurrent.STM
import GHC.Conc.Sync
import Control.Monad.IO.Class
import Contracts.Escrow.ContractCode.FreeLib
import System.IO.Unsafe

type Address = TVar Double
type Amount  = Double

data TransferOpF a where
    Deposit :: Address -> Amount -> a -> TransferOpF a
    Withdraw :: Address -> Amount -> a -> TransferOpF a
    CreateAddr :: Amount -> (Address -> a) -> TransferOpF a
    GetBalance :: Address -> (Amount -> a) -> TransferOpF a

deriving instance Functor TransferOpF

type TransferOpT = FreeT TransferOpF

deposit :: (MonadFree f m, TransferOpF :<: f) => Address -> Amount -> m ()
deposit addr amt = liftF . inj $ Deposit addr amt ()

withdraw ::  (MonadFree f m, TransferOpF :<: f) => Address -> Amount -> m ()
withdraw addr amt = liftF . inj $ Withdraw addr amt ()

createAddr :: (MonadFree f m, TransferOpF :<: f) => Amount -> m Address
createAddr amt = liftF . inj $ CreateAddr amt id

getBalance :: (MonadFree f m, TransferOpF :<: f) => Address -> m Amount
getBalance addr = liftF . inj $ GetBalance addr id

transfer :: (MonadIO m, MonadFree f m, TransferOpF :<: f) => Address -> Address -> Amount -> m ()
transfer f t a = liftIO $ atomically $ transferOpI $ do
    deposit t a
    withdraw f a

transferOpI :: TransferOpT STM r -> STM r
transferOpI a = do
    mr <- runFreeT a
    case mr of
        Pure r -> return r
        Free (Deposit addr amt cont) -> do
            bal <- readTVar addr
            writeTVar addr (bal + amt)
            transferOpI cont
        Free (Withdraw addr amt cont) -> do
            bal <- readTVar addr
            writeTVar addr (bal - amt)
            transferOpI cont
        Free (CreateAddr amt cont) -> do
            -- atomically $ do
            address <- newTVar amt
            transferOpI $ cont address

        Free (GetBalance addr cont) -> do
            bal <- readTVar addr
            -- putStrLn ("Balance is " ++ show bal)
            transferOpI $ cont bal

-- transferProg :: (MonadIO m, MonadFree f m, TransferOpF :<: f) => m ()
-- transferProg = do
--     from  <- createAddr 2000
--     to    <- createAddr 500
--     spare <- createAddr 1000
--     -- transfer from to 1500
--     a <- getBalance from
--     liftIO $ print a
--     getBalance to
--     return ()
--
-- instance MonadIO STM where
--     liftIO = unsafeIOToSTM
--
-- run = transferOpI transferProg
