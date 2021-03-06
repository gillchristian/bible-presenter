module Bible.Data.Slide where

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
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

derive instance genericSlideContent :: Generic SlideContent _

instance encodeJsonSlideContent :: EncodeJson SlideContent where
  encodeJson = genericEncodeJson

instance decodeJsonSlideContent :: DecodeJson SlideContent where
  decodeJson = genericDecodeJson

type Slide
  -- TODO: background support color
  = { background :: Maybe String
    , content :: SlideContent
    }
