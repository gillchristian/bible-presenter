module Bible.Capability.Resource.Bible where

import Prelude

import Bible.Data.Bible (Bible)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class Monad m <= ManageBible m where
  downloadBible :: String -> m (Maybe Bible)

instance manageBibleHalogenM :: ManageBible m => ManageBible (HalogenM st act slots msg m) where
  downloadBible = lift <<< downloadBible
