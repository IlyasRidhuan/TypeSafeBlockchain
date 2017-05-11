{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Blockchain.Patricia where

import Prelude hiding (lookup)
import Data.Maybe
import Control.Monad
import Data.List (sort)
import Data.Monoid
import Crypto.Hash
import Data.String
import Data.Binary
import GHC.Generics
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8 (pack)

newtype Key a = Key [a] deriving (Generic,Show,Eq,Ord)
instance Binary a => Binary (Key a)

data PatriciaTree k v = Empty| Root Hash [PatriciaTree k v]
                        | Node (Key k) (Value v) [PatriciaTree k v] deriving (Generic,Show,Eq,Ord)
instance (Binary k, Binary v) => Binary (PatriciaTree k v)

type Hash = Digest SHA256
instance Binary Hash where
    put hash = do
        put (0 :: Word8)
        put (convert hash :: ByteString)

    get = do
        tag <- getWord8
        case tag of
            0 -> do
                i <- get
                return $ fromMaybe (hashWith SHA256 ("0" :: ByteString)) (digestFromByteString (i :: ByteString ):: Maybe Hash)

data Value a = Ptr Hash | Value [a] deriving (Show, Eq,Ord,Generic)
instance Binary a => Binary (Value a)

-- type Breadcrumbs a = [Crumb a]
-- data Crumb a = LeftCrumb [a] Hash | RightCrumb [a] Hash deriving (Show)
-- type Focus k v = (PatriciaTree k v, Breadcrumbs a)

data Prefix a = FullPrefix [a] | PartialPrefix [a] deriving (Show)

hashNode :: (Show k, Show v) => [PatriciaTree k v] -> Hash
hashNode [] = hash ("" :: ByteString)
-- hashNode [Val x (Key y) _] = hash (B8.pack x <> B8.pack y)
hashNode xs = hash $ foldr (><) ("" :: ByteString) xs

(><) :: (Show k, Show v) => PatriciaTree k v -> ByteString -> ByteString
Node (Key x) (Ptr y) _ >< str = B8.pack (show x) <> B8.pack (show y) <> str
Node (Key x) (Value y) _ >< str = B8.pack (show x) <> B8.pack (show y) <> str


singleton :: PatriciaTree k v
singleton = Root (hash ("0"::ByteString)) [Empty]

insertPatriciaTree :: (Eq k, Eq v, Show k, Show v)=> PatriciaTree k v -> [k] -> [v] -> PatriciaTree k v
insertPatriciaTree (Root h [Empty]) key value = Root h [Node (Key key) (Value value) [Empty]]
insertPatriciaTree (Root h trees) key value = case prefixMap trees key of
    [(pt,Just (FullPrefix s))] ->
        Root h $ (\x -> if pt == x then insertPatriciaTree x (drop (length s) key) value else x) <$> trees
    [(pt@(Node (Key v) k subtrees), Just (PartialPrefix s))] ->
        Root h $ (\x -> if pt == x then Node (Key s) k [Node (Key (drop (length s) v)) k subtrees, Node (Key (drop (length s) key)) (Value value) [Empty] ] else x) <$> trees

    _ -> Root h $ Node (Key key) (Value value) [Empty] : trees

insertPatriciaTree (Node (Key n) k [Empty]) key value =
    Node (Key n) (Ptr $ hashNode [Node (Key key) (Value value) [Empty]] ) [Node (Key key) (Value value) [Empty]]
insertPatriciaTree (Node (Key n) k trees) key value = case prefixMap trees key of
    [(pt,Just (FullPrefix s))] ->
        Node (Key n) k $ (\x -> if pt == x then insertPatriciaTree x (drop (length s) key) value else x) <$> trees
    [(pt@(Node (Key v) k subtrees), Just (PartialPrefix s))] ->
        Node (Key n) k $ (\x -> if pt == x then Node (Key s) k [Node (Key (drop (length s) v)) k subtrees, Node (Key (drop (length s) key)) (Value value) [Empty] ] else x) <$> trees

    _ -> Node (Key n) k $ Node (Key key) (Value value) [Empty] : trees

reHashTree :: (Show k, Show v,Ord v,Ord k) => PatriciaTree k v -> PatriciaTree k v
reHashTree leaf@(Root h [Empty]) = leaf
reHashTree leaf@(Node (Key n) k [Empty]) = leaf
reHashTree (Root h trees) = Root (hashNode newTree) newTree
    where
        newTree = reHashTree <$> sort trees

reHashTree (Node (Key n) k trees) = Node (Key n) (Ptr $ hashNode newTree) newTree
    where
        newTree = reHashTree <$> sort trees

getRoot :: PatriciaTree k v -> Maybe Hash
getRoot (Root h _) = Just h
getRoot _          = Nothing

getValue :: (Eq k, Eq v) => PatriciaTree k v -> Maybe [v]
getValue (Node _ (Value x) _ ) = Just x
getValue _                = Nothing

getTree :: (Eq k, Eq v) => (PatriciaTree k v, [Hash]) -> [k] -> (PatriciaTree k v, [Hash])
getTree node [] = node
getTree (Root h trees,bs) key = case prefixMap trees key of
    [(pt, Just (FullPrefix s))] -> getTree (pt,h:bs) $ drop (length s) key
    [(pt, Just (PartialPrefix s))] -> getTree (pt,h:bs) $ drop (length s) key
    _                       -> (Empty, [])
getTree (Node (Key n) (Ptr k) trees,bs) key = case prefixMap trees key of
    [(pt, Just (FullPrefix s))] -> getTree (pt,k:bs) $ drop (length s) key
    [(pt, Just (PartialPrefix s))] -> getTree (pt,k:bs) $ drop (length s) key
    _                       -> (Empty, [])

update :: (Eq k, Eq v) => ([v] -> [v]) -> [k] -> PatriciaTree k v -> PatriciaTree k v
update f [] root@(Root h ts) = root
update f [] (Node (Key v) (Value k) ts) = Node (Key v) (Value $ f k) ts
update f key (Root h trees) = case prefixMap trees key of
    [(pt, Just (FullPrefix s))] ->
        Root h $ (\x -> if pt == x then update f (drop (length s) key) x else x) <$> trees

    [(pt, Just (PartialPrefix s))] ->
        Root h $ (\x -> if pt == x then update f (drop (length s) key) x else x) <$> trees

    _                       -> Empty
update f key (Node (Key v) k trees) = case prefixMap trees key of
    [(pt, Just (FullPrefix s))] ->
        Node (Key v) k $ (\x -> if pt == x then update f (drop (length s) key) x else x) <$> trees

    [(pt, Just (PartialPrefix s))] ->
        Node (Key v) k $ (\x -> if pt == x then update f (drop (length s) key) x else x) <$> trees

    _                       -> Empty

prefixMap :: (Eq k, Eq v) => [PatriciaTree k v] -> [k] -> [(PatriciaTree k v, Maybe(Prefix k))]
prefixMap trees str = filter (isJust . snd ) $ (\x -> (x,findPrefix x str)) <$> trees

slowPrefix :: (Eq a) => [a] -> [a] -> [a]
slowPrefix _ [] = []
slowPrefix [] _ = []
slowPrefix (x:xs) (y:ys)
    | x == y = x:slowPrefix xs ys
    | otherwise = []


findPrefix :: (Eq k, Eq v) => PatriciaTree k v -> [k] -> Maybe (Prefix k)
findPrefix Empty _ = Nothing
findPrefix (Node (Key n) _ _) str
    | pfx == n = Just $ FullPrefix pfx
    | null pfx = Nothing
    | otherwise = Just $ PartialPrefix pfx

    where pfx = slowPrefix n str
