{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.HashMap.Generator where

import Prelude hiding (lookup)
import qualified Data.HashMap.Strict as H
import Data.Hashable 
import GHC.Generics
import qualified Data.Text as T
import Language.Haskell.TH
import Data.Proxy


-- Requirements 
-- Completely reified
data AppMailboxes = LocationToEdit (Maybe Int)
                                    |  PartsToDisplay String
   deriving (Eq, Ord, Show, Generic)


-- keyLocationToEdit  = LocationToEdit Nothing
-- keyPartsToDisplay = PartsToDisplay ""



instance SelfHash AppMailboxes where

-- ========================================================================================
-- { Exported Things: }

type SelfMap a = H.HashMap SelfKey a

fromList :: (SelfHash a, Eq a) => [a] -> SelfMap a
fromList v' = foldr mapMaker H.empty v'
  where
    mapMaker v m = H.insert (selfKey v) v m

lookup :: (SelfHash a) => Name -> SelfMap a -> Maybe a
lookup n m = H.lookup selfKey' m
  where
    selfKey' = SelfKey . T.pack . nameBase $ n

insert :: (SelfHash a) =>  a -> SelfMap a -> SelfMap a
insert v m = H.insert (selfKey v) v m

-- ONLY EXPORT THE TYPE CONSTRUCTOR, NOT THE DATA CONSTRUCTOR
newtype SelfKey = SelfKey {unSelfKey :: T.Text}
  deriving (Eq, Ord, Show, Hashable)

class SelfHash a where
    selfHashWithSalt :: Int -> a -> Int
    default selfHashWithSalt :: (Generic a, GSelfHash (Rep a)) => Int -> a -> Int
    selfHashWithSalt = genericSelfHashWithSalt
    selfKey :: a -> SelfKey
    default selfKey :: (Generic a, GSelfHash (Rep a)) => a -> SelfKey
    selfKey = genericSelfKey
-- ========================================================================================

-- ==========================================================================================
-- { Non-exported instances: }

instance (GSelfHash rep) => GSelfHash (M1 i t rep) where
    gSelfHashWithSalt i = gSelfHashWithSalt i . unM1
    gSelfKey = gSelfKey .unM1


instance GSelfHash (U1) where
    gSelfHashWithSalt i _ = hashWithSalt i $ SelfKey ""
    gSelfKey _ = SelfKey ""

-- Needed for safe head access
safeHead :: forall t. t -> [t] -> t
safeHead d [] = d
safeHead _ (x:_) = x

instance  (CN l, CN r) => GSelfHash (l :+: r) where
   gSelfHashWithSalt i (L1 _) = hashWithSalt i .  SelfKey . T.pack . safeHead "" . constructorNames $ (Proxy :: Proxy l)
   gSelfHashWithSalt i (R1 _) = hashWithSalt i .  SelfKey . T.pack . safeHead "" . constructorNames $ (Proxy :: Proxy r)
   gSelfKey (L1 _) = SelfKey . T.pack . safeHead "" . constructorNames $ (Proxy :: Proxy l)
   gSelfKey (R1 _) = SelfKey . T.pack . safeHead "" . constructorNames $ (Proxy :: Proxy r)

instance CN f => CN (D1 c f) where
  constructorNames _ = constructorNames (Proxy :: Proxy f)

instance (CN x, CN y) => CN (x :+: y) where
  constructorNames _ = constructorNames (Proxy :: Proxy x) ++ constructorNames (Proxy :: Proxy y)

instance Constructor c => CN (C1 c f) where
  constructorNames _ = [conName (undefined :: t c f a)]
-- ==========================================================================================

-- ==========================================================================================
-- { Miscellaneous Functions: }

genericSelfHashWithSalt :: (Generic a, GSelfHash (Rep a)) => Int -> a -> Int
genericSelfHashWithSalt i = gSelfHashWithSalt i .from

genericSelfKey :: (Generic a, GSelfHash (Rep a)) => a -> SelfKey
genericSelfKey = gSelfKey .from
-- ==========================================================================================

-- ==========================================================================================
-- { Non-exported classes :}

class CN (f :: * -> *) where
 constructorNames :: proxy f -> [String]

class GSelfHash (f :: * -> *) where
    gSelfHashWithSalt :: Int -> f a -> Int
    gSelfKey :: f a -> SelfKey

-- | Get the name of the constructor of a sum datatype.
class GetConName f where
    getConName :: f a -> String
-- ==========================================================================================