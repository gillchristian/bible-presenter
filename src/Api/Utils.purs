-- | This module exports various utilities for working with a REST API and Json. It also provides
-- | a few helpers shared among requests which I found useful when implementing the production
-- | monad, `Bible.AppM`.
module Bible.Api.Utils where

import Prelude

import Affjax (request)
import Bible.Api.Request (RequestOptions, defaultRequest)
import Bible.Capability.LogMessages (class LogMessages, logError)
import Bible.Capability.Now (class Now)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode as CD
import Data.Bifunctor (rmap)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)

-- | This function performs a request that does not require authentication by pulling the base URL
-- | out of the app environment and running an asynchronous request. This function only requires the
-- | `baseUrl` field from the app environment. See `Bible.AppM` for examples of this in action.
mkRequest
  :: forall m
   . MonadAff m
  => RequestOptions
  -> m (Maybe Json)
mkRequest opts = do
  response <- liftAff $ request $ defaultRequest opts
  pure $ hush $ rmap _.body response

-- | This small utility decodes JSON and logs any failures that occurred, returning the parsed
-- | value only if decoding succeeded. This utility makes it easy to abstract the mechanices of
-- | dealing with malformed responses. See `Bible.AppM` for examples of this in practice.
decode :: forall m a. LogMessages m => Now m => CA.JsonCodec a -> Maybe Json -> m (Maybe a)
decode _ Nothing = logError "Response malformed" *> pure Nothing
decode codec (Just json) = case CA.decode codec json of
  Left err -> logError (CA.printJsonDecodeError err) *> pure Nothing
  Right response -> pure (Just response)

-- | Similar to decode_ but expects a `DecodeJson` instance instead of a
-- | decoder.
decode_ :: forall m a. LogMessages m => Now m => DecodeJson a => Maybe Json -> m (Maybe a)
decode_ Nothing = logError "Response malformed" *> pure Nothing
decode_ (Just json) = case CD.decodeJson json of
  Left err -> logError (CD.printJsonDecodeError err) *> pure Nothing
  Right response -> pure (Just response)
