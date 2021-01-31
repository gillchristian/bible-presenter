module Bible.Capability.Resource.List where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class Monad m <= ManageList m where
  createList :: String -> m (Maybe String)

instance manageListHalogenM :: ManageList m => ManageList (HalogenM st act slots msg m) where
  createList = lift <<< createList
