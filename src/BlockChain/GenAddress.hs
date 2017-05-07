{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Blockchain.GenAddress where

import Crypto.PubKey.ECC.Types
import Crypto.PubKey.ECC.Generate
import Crypto.PubKey.ECC.ECDSA
import Crypto.Random
import Crypto.Number.Generate
import Crypto.Hash
import Data.Monoid
import qualified Data.ByteString.Char8 as B8

newtype AccountAddress = AccountAddress {unAA :: B8.ByteString} deriving (Show)

genKey :: IO AccountAddress
genKey = do
    (PublicKey _ (Point x y),prv) <- generate $ getCurveByName SEC_p256k1
    let fullAddr = hashWith SHA3_256 $ B8.pack $ show x <> show y
    return $ AccountAddress$ B8.drop 24 $ B8.pack $ show fullAddr
