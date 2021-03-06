module Bible.Data.Presentation where

import Prelude

import Bible.Data.Slide (Slide)
import Bible.Foreign.BroadcastChannel as BroadcastChannel
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen.Query.EventSource as HES

--------------------------------------------------------------------------------
-- Presenter -------------------------------------------------------------------
--------------------------------------------------------------------------------

data ToPresenter
  = SetSlide Slide

derive instance genericToPresenter :: Generic ToPresenter _

instance encodeJsonToPresenter :: EncodeJson ToPresenter where
  encodeJson = genericEncodeJson

instance decodeJsonToPresenter :: DecodeJson ToPresenter where
  decodeJson = genericDecodeJson

--------------------------------------------------------------------------------
-- Coordinator -----------------------------------------------------------------
--------------------------------------------------------------------------------

data ToCoordinator
  = GetState

derive instance genericToCoordinator :: Generic ToCoordinator _

instance encodeJsonToCoordinator :: EncodeJson ToCoordinator where
  encodeJson = genericEncodeJson

instance decodeJsonToCoordinator :: DecodeJson ToCoordinator where
  decodeJson = genericDecodeJson

--------------------------------------------------------------------------------
-- Channel ---------------------------------------------------------------------
--------------------------------------------------------------------------------

type Channel msg
  = { close :: Effect Unit
    , postMessage :: msg -> Effect Unit
    }

mkChannel ::
  forall act m outgoing incoming.
  MonadAff m =>
  DecodeJson incoming =>
  EncodeJson outgoing =>
  String ->
  (incoming -> act) ->
  Effect { channel :: Channel outgoing , source :: HES.EventSource m act }
mkChannel name onMsg = do
  bc <- BroadcastChannel.createChannel name

  let source =
        HES.effectEventSource \emitter -> do
          bc.setOnMessage $ \json -> for_ (decodeJson json) \msg -> HES.emit emitter $ onMsg msg
          pure $ HES.Finalizer bc.close

      channel = { close: bc.close, postMessage: bc.postMessage <<< encodeJson }

  pure $ { channel, source }
