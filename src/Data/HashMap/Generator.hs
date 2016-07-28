module Data.HashMap.Generator where

import qualified Data.HashMap.Strict as H
import Data.Hashable
import Control.Lens
-- Requirements 
-- Completely reified
data AppMailboxes = LocationToEdit (Maybe Int)
                                    |  PartsToDisplay String
   deriving (Eq, Ord, Show)

instance Hashable AppMailboxes where
    hashWithSalt i (LocationToEdit _) = hashWithSalt i "LocationToEdit"
    hashWithSalt i (PartsToDisplay _) = hashWithSalt i "PartsToDisplay"

keyLocationToEdit  = LocationToEdit Nothing
keyPartsToDisplay = PartsToDisplay ""

passableMailbox :: H.HashMap AppMailboxes AppMailboxes
passableMailbox = H.fromList [(LocationToEdit  (Just 4), LocationToEdit (Just 3))
                                                       , (PartsToDisplay "Brent", PartsToDisplay "Brent")
                                                       ]



-- submitEditIO :: BackofficeInterface -> STMMailbox (Maybe (Entity Location)) -> STMMailbox (S.Seq (PartByLocation)) -> STMMailbox Bool -> STMMailbox Text -> STMMailbox (S.Seq (Entity Part)) -> STMMailbox Text -> STMMailbox (Maybe Double) -> STMMailbox (Maybe Double) -> STMMailbox Bool -> STMMailbox (Maybe (Entity Part)) -> STMMailbox (H.HashMap PartId Bool) -> STMMailbox FormMode -> STMMailbox (Maybe (PartByLocation))  -> IO ()
-- submitEditIO interface mayLocToEditMb partsToDisplayMb partFormShowMb partFormSearchFieldMb dropDownPartsMb binNumberEntryMb binQtyEntryMb reservedQtyEntryMb partIsSelectedMb maySelectedPartMb hashMapEditValuesMb partFormModeMb mayPartToEditMb = do