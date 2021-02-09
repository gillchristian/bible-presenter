module Bible.Capability.LocalStorage where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class Monad m <= LocalStorage m where
  getItem :: forall a. DecodeJson a => String -> m (Maybe a)
  setItem :: forall a. EncodeJson a => String -> a -> m Unit
  removeItem :: String -> m Unit

instance localStorageHalogenM :: LocalStorage m => LocalStorage (HalogenM st act slots msg m) where
  getItem = lift <<< getItem
  setItem s a = lift $ setItem s a
  removeItem = lift <<< removeItem
