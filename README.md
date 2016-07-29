# hash-map-generator

The hashmap generator creates hash maps that are self hashed from sum types.
This can be useful for some dynamic programming applications.  
Notably, the problem of passing around lots of mailboxes of varying type to different places in a codebase

## Installation

``` cabal install hash-map-generator```

## Usage

``` haskell
{-# LANGUAGE DeriveGeneric   #-} -- to derive the instances
{-# LANGUAGE TemplateHaskell #-} -- for the comma notation


import           Data.HashMap.Generator       -- For the types
import qualified Data.HashMap.Generator as G  -- Avoid naming collisions


import           GHC.Generics                 -- Build Generics


-- | An example sum type
data AppMailboxes = LocationToEdit (Maybe Int)
                |  PartsToDisplay String
   deriving (Eq, Ord, Show, Generic)

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


```

lookupParts to display is an example of using the generated map `exampleAppMailbox` to retrieve
a piece of data.
```
λ> lookupPartsToDisplay
Just (PartsToDisplay "apart") lookupPartsToDisplay
```

## How to run tests

```
cabal install --enable-tests
cabal test
```

