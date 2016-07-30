{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.HashMap.GeneratorSpec (tests) where

import           Data.HashMap.Generator
import qualified Data.HashMap.Generator as G


import           Test.Tasty

import qualified Data.HashMap.Strict as HM

import           Test.Tasty.HUnit

import           GHC.Generics

import           Control.Lens


-- | An example sum type
data AppMailboxes = LocationToEdit (Maybe Int)
                |  PartsToDisplay String
                |  WondersToSeek  Int
                |  LivesToLive    (Either String Int)
   deriving (Eq, Ord, Show, Generic)

makePrisms ''AppMailboxes

instance SelfHash AppMailboxes where

-- | Build a SelfMap similiarly to a regular Hashmap, however there is no Key
-- field because that is what is generated.
-- notice the order in expectedKeys doesn't match the order in the list
-- That is because when it comes back from the list the hash order is different  
exampleAppMailbox :: SelfMap AppMailboxes
exampleAppMailbox = fromList [  LocationToEdit (Just 1)
                            , PartsToDisplay "apart"
                            , WondersToSeek 3
                            , LivesToLive (Left "one life only")]
                            

exampleAppMailboxFromInserts :: SelfMap AppMailboxes
exampleAppMailboxFromInserts = G.insert  (LocationToEdit (Just 1))           $
                            G.insert (PartsToDisplay "apart")             $
                            G.insert (WondersToSeek 3)                    $                            
                            G.insert (LivesToLive (Left "one life only")) $ HM.empty

expectedKeys :: [SelfKey AppMailboxes]
expectedKeys =  [  key 'LocationToEdit
                , key 'WondersToSeek
                , key 'PartsToDisplay
                , key 'LivesToLive]

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

  , testCase "keys example map should match expeted keys"  (
       (HM.keys exampleAppMailbox) `compare` expectedKeys @?= EQ )
  , testCase "keys example map from insertshould match expeted keys"  (
       (HM.keys exampleAppMailboxFromInserts) `compare` expectedKeys @?= EQ )       
  ]
