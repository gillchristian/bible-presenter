module Bible.Foreign.BroadcastChannel where

import Prelude

import Data.Argonaut.Core (Json)
import Effect (Effect)

type BroadcastChannel
  = { postMessage :: Json -> Effect Unit
    , setOnMessage :: (Json -> Effect Unit) -> Effect Unit
    , close :: Effect Unit
    }

foreign import createChannel :: String -> Effect BroadcastChannel
