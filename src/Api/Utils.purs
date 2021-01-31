-- | This module exports various utilities for working with a REST API and Json. It also provides
-- | a few helpers shared among requests which I found useful when implementing the production
-- | monad, `Bible.AppM`.
module Bible.Api.Utils where

import Prelude

import Affjax (request)
import Bible.Api.Request (BaseURL, RequestOptions, defaultRequest)
import Bible.Capability.LogMessages (class LogMessages, logError)
import Bible.Capability.Now (class Now)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (rmap)
import Data.Codec.Argonaut (JsonCodec, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)

-- | This function performs a request that does not require authentication by pulling the base URL
-- | out of the app environment and running an asynchronous request. This function only requires the
-- | `baseUrl` field from the app environment. See `Bible.AppM` for examples of this in action.
mkRequest
  :: forall m r
   . MonadAff m
  => MonadAsk { baseUrl :: BaseURL | r } m
  => RequestOptions
  -> m (Maybe Json)
mkRequest opts = do
  { baseUrl } <- ask
  response <- liftAff $ request $ defaultRequest baseUrl opts
  pure $ hush $ rmap _.body response

-- | This small utility decodes JSON and logs any failures that occurred, returning the parsed
-- | value only if decoding succeeded. This utility makes it easy to abstract the mechanices of
-- | dealing with malformed responses. See `Bible.AppM` for examples of this in practice.
decode :: forall m a. LogMessages m => Now m => JsonCodec a -> Maybe Json -> m (Maybe a)
decode _ Nothing = logError "Response malformed" *> pure Nothing
decode codec (Just json) = case CA.decode codec json of
  Left err -> logError (printJsonDecodeError err) *> pure Nothing
  Right response -> pure (Just response)
