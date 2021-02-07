module Bible.Api.Request
  ( BaseURL(..)
  , RequestMethod(..)
  , RequestOptions(..)
  , defaultRequest
  ) where

import Prelude

import Affjax (Request)
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Bible.Api.Endpoint (Endpoint, endpointCodec)
import Routing.Duplex (print)

newtype BaseURL
  = BaseURL String

data RequestMethod
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete

type RequestOptions
  = { baseUrl :: BaseURL
    , endpoint :: Endpoint
    , method :: RequestMethod
    }

defaultRequest :: RequestOptions -> Request Json
defaultRequest { baseUrl: BaseURL baseUrl, endpoint, method } =
  { method: Left method
  , url: baseUrl <> print endpointCodec endpoint
  , content: RB.json <$> body
  , headers: []
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: RF.json
  }
  where
  Tuple method body = case method of
    Get -> Tuple GET Nothing
    Post b -> Tuple POST b
    Put b -> Tuple PUT b
    Delete -> Tuple DELETE Nothing
