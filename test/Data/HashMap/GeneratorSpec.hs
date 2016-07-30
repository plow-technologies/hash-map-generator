{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.HashMap.GeneratorSpec (tests) where

import           Data.HashMap.Generator
import qualified Data.HashMap.Generator as G


import           Test.Tasty

import           Test.Tasty.HUnit

import           GHC.Generics

import           Control.Lens


-- | An example sum type
data AppMailboxes = LocationToEdit (Maybe Int)
                |  PartsToDisplay String
   deriving (Eq, Ord, Show, Generic)

makePrisms ''AppMailboxes

instance SelfHash AppMailboxes where

-- | Build a SelfMap similiarly to a regular Hashmap, however there is no Key
-- field because that is what is generated.
exampleAppMailbox :: SelfMap AppMailboxes
exampleAppMailbox = fromList [LocationToEdit (Just 1), PartsToDisplay "apart"]


-- | Notice, to access the fields we use '\'' to grab the Template Haskell Name
-- This means {-# LANGUAGE TemplateHaskell #-} is required
lookupLocationToEdit :: Maybe AppMailboxes
lookupLocationToEdit = G.lookup 'LocationToEdit exampleAppMailbox

lookupPartsToDisplay :: Maybe AppMailboxes
lookupPartsToDisplay = G.lookup 'PartsToDisplay exampleAppMailbox



-- | Using lens should get you to the bottom
useLensToLookupPartsToDisplay :: Maybe String
useLensToLookupPartsToDisplay = exampleAppMailbox ^? ix (G.key 'PartsToDisplay ) . _PartsToDisplay




-- Tests
tests :: TestTree
tests = testGroup "Tests" [unitTests]



unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Should retrive Location to Edit" (
       lookupLocationToEdit `compare`  Just (LocationToEdit (Just 1)) @?= EQ)

  , testCase "Should retrive Part to Display" (
       lookupPartsToDisplay `compare` Just (PartsToDisplay "apart") @?= EQ)
  , testCase "Should retrive Part to Display Using lens" (
       useLensToLookupPartsToDisplay `compare` Just "apart" @?= EQ)       
  ]
