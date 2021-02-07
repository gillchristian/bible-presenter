module Bible.Data.Presentation where

import Prelude

import Bible.Data.Slide (Slide)
import Bible.Foreign.BroadcastChannel as BroadcastChannel
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Either (Either(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen.Query.EventSource as HES

--------------------------------------------------------------------------------
-- Presenter -------------------------------------------------------------------
--------------------------------------------------------------------------------

data ToPresenter
  = SetSlide Slide

instance encodeJsonToPresenter :: EncodeJson ToPresenter where
  encodeJson (SetSlide slide) =
    "type" := "SetSlide"
      ~> "value" := slide
      ~> jsonEmptyObject

instance decodeJsonToPresenter :: DecodeJson ToPresenter where
  decodeJson json = do
    obj <- decodeJson json
    type_ <- obj .: "type"

    case type_ of
      "SetSlide" -> do
        value <- obj .: "value"
        Right $ SetSlide value
      _ -> Left $ AtKey "type" $ UnexpectedValue $ encodeJson type_

--------------------------------------------------------------------------------
-- Coordinator -----------------------------------------------------------------
--------------------------------------------------------------------------------

data ToCoordinator
  = GetState

instance encodeJsonToCoordinator :: EncodeJson ToCoordinator where
  encodeJson GetState =
    "type" := "GetState"
      ~> jsonEmptyObject

instance decodeJsonToCoordinator :: DecodeJson ToCoordinator where
  decodeJson json = do
    obj <- decodeJson json
    type_ <- obj .: "type"

    case type_ of
      "GetState" -> Right GetState
      _ -> Left $ AtKey "type" $ UnexpectedValue $ encodeJson type_

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
