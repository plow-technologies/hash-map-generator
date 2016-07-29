{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.HashMap.GeneratorSpec (tests) where

import Data.HashMap.Generator
import qualified Data.HashMap.Generator as G


import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import GHC.Generics

import Data.Ord

tests :: TestTree
tests = testGroup "Tests" [unitTests]



unitTests = testGroup "Unit tests"
  [ testCase "Should retrive Location to Edit" (
      lookupLocationToEdit `compare` (Just (LocationToEdit (Just 1))) @?= EQ)

  -- the following test does not hold
  , testCase "Should retrive Part to Display" (
      lookupPartsToDisplay `compare` (Just (PartsToDisplay ("apart"))) @?= EQ)
  ]

  -- Requirements 
-- Completely reified
data AppMailboxes = LocationToEdit (Maybe Int)
                                    |  PartsToDisplay String
   deriving (Eq, Ord, Show, Generic)

instance SelfHash AppMailboxes where

exampleAppMailbox :: SelfMap AppMailboxes
exampleAppMailbox = fromList [LocationToEdit (Just 1), PartsToDisplay "apart"]

lookupLocationToEdit :: Maybe AppMailboxes
lookupLocationToEdit = G.lookup 'LocationToEdit exampleAppMailbox

lookupPartsToDisplay :: Maybe AppMailboxes
lookupPartsToDisplay = G.lookup 'PartsToDisplay exampleAppMailbox