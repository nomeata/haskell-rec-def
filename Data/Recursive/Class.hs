{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Recursive.Class where

class Eq (Val a) => Order a where
    type Val a
    bottom :: Val a
