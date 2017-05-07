{-# LANGUAGE DeriveFunctor, GADTs, StandaloneDeriving,TypeOperators,FlexibleContexts#-}
module Contracts.Escrow.ContractCode.StorageLib where

import Control.Monad.Trans.Free
import Control.Monad.IO.Class
import Data.Map
import Contracts.Escrow.ContractCode.FreeLib
import Prelude hiding (lookup)
import System.IO.Unsafe (unsafePerformIO)

data StorageF a where
    CreateStore :: Ord k => (Map k b -> a) -> StorageF a
    InsertElement :: Ord k => k -> b -> Map k b -> (Map k b -> a) -> StorageF a
    DeleteElement :: Ord k => k -> Map k b -> (Map k b -> a) -> StorageF a
    GetElement :: Ord k => k -> Map k b -> (b -> a) -> StorageF a
    ModifyElement :: Ord k => (b -> b) -> k -> Map k b -> (Map k b -> a) -> StorageF a

deriving instance Functor StorageF


type StorageT = FreeT StorageF

createStore :: (MonadFree f m, StorageF :<: f, Ord k) => m (Map k b)
createStore = liftF . inj $ CreateStore id

insertElement :: (MonadFree f m, StorageF :<: f, Ord k) => k -> b -> Map k b -> m (Map k b)
insertElement k v m = liftF . inj $ InsertElement k v m id

deleteElement :: (MonadFree f m, StorageF :<: f, Ord k) => k -> Map k b -> m (Map k b)
deleteElement k m = liftF . inj $ DeleteElement k m id

getElement :: (MonadFree f m, StorageF :<: f, Ord k) => k -> Map k b -> m b
getElement k m = liftF . inj $ GetElement k m id

modifyElement :: (MonadFree f m, StorageF :<: f, Ord k) => (b -> b) -> k -> Map k b -> m (Map k b)
modifyElement f k m = liftF . inj $ ModifyElement f k m id


storageI :: StorageT IO r -> IO r
storageI a = do
    mr <- runFreeT a
    case mr of
        Pure r -> return r
        Free (CreateStore cont) -> do
            let store = empty
            storageI $ cont store

        Free (InsertElement key value m cont) -> do
            let store' = insert key value m
            storageI $ cont store'

        Free (DeleteElement key m cont) -> do
            let store' = delete key m
            storageI $ cont store'

        Free (GetElement key m cont) -> do
            let Just val = lookup key m
            storageI $ cont val

        Free (ModifyElement f key m cont) -> do
            let store' = adjust f key m
            storageI $ cont store'

-- testStorage :: (MonadIO m, MonadFree f m, StorageF :<: f) => m ()
-- testStorage = do
--     m <- createStore
--     m' <- insertElement "hello" 10 m
--     str <- getElement "hello" m'
--     -- liftIO $ print str
--     m'' <- modifyElement (+10) "hello" m'
--     str2 <- getElement "hello" m''
--     liftIO $ print str2
--
--     -- unsafePerformIO $ liftIO $ print str
--     return ()


-- runS = storageI testStorage
