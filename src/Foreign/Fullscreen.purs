module Bible.Foreign.Fullscreen
  ( checkFullScreen
  , exitFullScreen
  , requestFullScreen
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect.Aff (Aff)
import Web.DOM (Document)
import Web.HTML.HTMLElement (HTMLElement)

foreign import requestFullScreen_ :: HTMLElement -> Promise Boolean

requestFullScreen :: HTMLElement -> Aff Boolean
requestFullScreen = Promise.toAff <<< requestFullScreen_

foreign import exitFullScreen_ :: Document -> Promise Boolean

exitFullScreen :: Document -> Aff Boolean
exitFullScreen = Promise.toAff <<< exitFullScreen_

-- TODO: this should be: Document -> Effect Boolean
foreign import checkFullScreen :: Document -> Boolean
