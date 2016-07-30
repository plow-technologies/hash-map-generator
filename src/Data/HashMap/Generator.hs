{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{- |
Module      : Data.HashMap.Generator
Description : Generate 'SelfMap' a hashmap which is self keyed
Copyright   : Plow Technologies LLC
License     : MIT License

Maintainer  : Scott Murphy

The main purpose of this is to allow somewhat dynamic
typing among a set of options going into a function.



| -}


module Data.HashMap.Generator ( lookup
                            , fromList
                            , insert
                            , key
                            , SelfMap
                            , SelfKey
                            , SelfHash (..)) where


import           Data.Hashable
import qualified Data.HashMap.Strict as H
import           Data.Proxy
import qualified Data.Text           as T
import           GHC.Generics
import           Language.Haskell.TH
import           Prelude             hiding (lookup)



-- | 'SelfMap' is a 'HashMap' whose Key is derived from the type itself
-- It is expected to be a SumType with single valued data constructors.
type SelfMap a = H.HashMap (SelfKey a) a

-- | The key for a 'SelfMap' is not available to use except through
-- 'lookup'
newtype SelfKey p = SelfKey {_unSelfKey :: T.Text}
  deriving (Eq, Ord, Show, Hashable)





-- | Build a SelfMap from a list of values 
-- >>> fromList [RightConstructor 1, LeftOne "apart"]
fromList :: (SelfHash a, Eq a) => [a] -> SelfMap a
fromList = foldr mapMaker H.empty 
  where
    mapMaker v = H.insert (selfKey v) v




-- | find a field in a 'SelfMap' using the Template Haskell Name
-- >>> lookup 'RightConstructor exampleSelfMap
lookup :: (SelfHash a) => Name -> SelfMap a -> Maybe a
lookup n = H.lookup selfKey' 
  where
    selfKey' = SelfKey . T.pack . nameBase $ n


-- |Useful for retrieving the 'SelfKey' for use in alternate hashmap routines
key :: (SelfHash a) => Name -> (SelfKey a)
key n = selfKey'
  where
    selfKey' = SelfKey . T.pack . nameBase $ n
-- | Extend a 'SelfMap' with a new piece of data
insert :: (SelfHash a) =>  a -> SelfMap a -> SelfMap a
insert v = H.insert (selfKey v) v 


{-|
SelfHash is designed to be used with a type with deriving (Generic )

@
data AppMailboxes = LocationToEdit (Maybe Int)
                 |  PartsToDisplay String
   deriving (Eq, Ord, Show, Generic)

instance SelfHash AppMailboxes where
@
|-}

class SelfHash a where
    selfHashWithSalt         :: Int -> a -> Int
    default selfHashWithSalt :: (Generic a, GSelfHash (Rep a)) => Int -> a -> Int
    selfHashWithSalt = genericSelfHashWithSalt
    

    selfKey         :: a -> (SelfKey a)
    default selfKey :: (Generic a, GSelfHash (Rep a)) => a -> (SelfKey a)
    selfKey = genericSelfKey



-- | Functions used to transport (GSelfHash * -> * ) into (SelfHash *)

genericSelfHashWithSalt :: (Generic a, GSelfHash (Rep a)) => Int -> a -> Int
genericSelfHashWithSalt i = gSelfHashWithSalt i .from

genericSelfKey :: (Generic a, GSelfHash (Rep a)) => a -> (SelfKey a)
genericSelfKey = gSelfKey .from













-- |Generically create a hashable 'SelfKey' from a sum type's
-- constructors 
class GSelfHash (f :: * -> *) where
    gSelfHashWithSalt :: Int -> f a      -> Int
    gSelfKey          :: f a -> (SelfKey a)





-- | Generic parts defined, :+: , M1, U1,
--   :*: is undefined intentionally
instance (GSelfHash rep) => GSelfHash (M1 i t rep) where
    gSelfHashWithSalt i = gSelfHashWithSalt i . unM1
    gSelfKey            = gSelfKey . unM1




instance GSelfHash U1 where
    gSelfHashWithSalt i _ = hashWithSalt i (SelfKey "")
    gSelfKey          _   = SelfKey ""


instance  (CN l, CN r) => GSelfHash (l :+: r) where
   gSelfHashWithSalt i      (L1 _) = buildHashFromProxy i  (Proxy :: Proxy l)
   gSelfHashWithSalt i      (R1 _) = buildHashFromProxy i  (Proxy :: Proxy r)
   gSelfKey          (L1 _)        = buildSelfKeyFromProxy (Proxy :: Proxy l)
   gSelfKey          (R1 _)        = buildSelfKeyFromProxy (Proxy :: Proxy r)




-- | Helper function, to retrieve names and hash them as 'SelfKey'
buildHashFromProxy :: CN f => Int -> proxy f -> Int
buildHashFromProxy i = hashWithSalt i .  buildSelfKeyFromProxy

buildSelfKeyFromProxy  :: CN f => proxy f -> (SelfKey a)
buildSelfKeyFromProxy = SelfKey . T.pack . safeHead "" . constructorNames

-- | Needed for safe head access
safeHead :: forall t. t -> [t] -> t
safeHead default' [ ]       = default'
safeHead _        (head':_) = head'









-- | Name Retrieval Class, to get necessary string
class CN (f :: * -> *) where
 constructorNames :: proxy f -> [String]


-- | Generic parts to run through layers of Generic Structure
instance CN f => CN (D1 c f) where
  constructorNames _ = constructorNames (Proxy :: Proxy f)

instance (CN x, CN y) => CN (x :+: y) where
  constructorNames _ = constructorNames (Proxy :: Proxy x) ++ constructorNames (Proxy :: Proxy y)

instance Constructor c => CN (C1 c f) where
  constructorNames _ = [conName (undefined :: t c f a)]



