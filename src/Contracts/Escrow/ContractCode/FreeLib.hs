{-# LANGUAGE TypeOperators, UndecidableInstances,DeriveFunctor,MultiParamTypeClasses,FlexibleInstances,FlexibleContexts #-}

module Contracts.Escrow.ContractCode.FreeLib where

infixr 5 :+:
data (f :+: g) e = Inl (f e) | Inr (g e) deriving (Show,Functor)

class (Functor sup, Functor sub) => sub :<: sup where
    inj :: sub a -> sup a
    prj :: sup a -> Maybe (sub a)

instance Functor f => f :<: f where
    inj = id
    prj = Just

instance (Functor f, Functor g) => f :<: (f :+: g) where
    inj = Inl
    prj (Inl x) = Just x
    prj (Inr y) = Nothing

instance {-# OVERLAPPABLE #-}(Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
    inj = Inr . inj
    prj (Inr x) = prj x
    prj (Inl x) = Nothing
