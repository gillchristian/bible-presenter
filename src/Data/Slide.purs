module Bible.Data.Slide where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Either (Either(..))
import Data.Maybe (Maybe)

type VerseSpec
  = { book :: String
    , chapter :: Int
    , verse :: Int
    , contents :: String
    }

data SlideContent
  = Still
  | Text String
  | Verse VerseSpec

instance encodeJsonSlideContent :: EncodeJson SlideContent where
  encodeJson Still =
    "type" := "Still"
      ~> jsonEmptyObject
  encodeJson (Text contents) =
    "type" := "Text"
      ~> "value" := contents
      ~> jsonEmptyObject
  encodeJson (Verse verse) =
    "type" := "Verse"
      ~> "value" := verse
      ~> jsonEmptyObject

instance decodeJsonSlideContent :: DecodeJson SlideContent where
  decodeJson json = do
    obj <- decodeJson json
    type_ <- obj .: "type"

    case type_ of
      "Still" -> Right Still
      "Text" -> do
        value <- obj .: "value"
        Right $ Text value
      "Verse" -> do
        value <- obj .: "value"
        Right $ Verse value
      _ -> Left $ AtKey "type" $ UnexpectedValue $ encodeJson type_

type Slide
  -- TODO: background support color
  = { background :: Maybe String
    , content :: SlideContent
    }
