module Bible.AppM where

import Prelude

import Bible.Api.Endpoint (Endpoint(..))
import Bible.Api.Request (RequestMethod(..))
import Bible.Api.Utils (decode_, mkRequest)
import Bible.Capability.Clipboard (class Clipboard)
import Bible.Capability.LocalStorage (class LocalStorage)
import Bible.Capability.LogMessages (class LogMessages)
import Bible.Capability.Navigate (class Navigate, locationState, navigate)
import Bible.Capability.Now (class Now)
import Bible.Capability.Resource.Bible (class ManageBible)
import Bible.Capability.Resource.List (class ManageList)
import Bible.Data.Log as Log
import Bible.Data.Route as Route
import Bible.Env (Env, LogLevel(..))
import Bible.Foreign.Clipboard as ForeignClipboard
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Core as CA
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (hush)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Routing.Duplex (print)
import Type.Equality (class TypeEquals, from)
import Web.Event.Event (preventDefault)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage as WStorage

newtype AppM a
  = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance cliboardAppM :: Clipboard AppM where
  writeText = liftAff <<< ForeignClipboard.writeText

instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

instance localStorageAppM :: LocalStorage AppM where
  getItem key = do
    str <- liftEffect $ WStorage.getItem key =<< localStorage =<< window
    pure $ (hush <<< decodeJson) =<< (hush <<< jsonParser) =<< str
  setItem key a =
    liftEffect $ WStorage.setItem key value =<< localStorage =<< window
    where value = stringify $ encodeJson a
  removeItem key =
    liftEffect $ WStorage.removeItem key =<< localStorage =<< window

instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do
    env <- ask
    liftEffect case env.logLevel, Log.reason log of
      Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.message log

instance navigateAppM :: Navigate AppM where
  locationState = liftEffect =<< _.nav.locationState <$> ask

  navigate route = do
    { pushState } <- asks _.nav
    { state } <- locationState
    liftEffect $ pushState state $ print Route.routeCodec $ route

  navigate_ event route = do
    liftEffect $ preventDefault event
    navigate route

instance manageListAppM :: ManageList AppM where
  createList _ = do
    { baseUrl } <- ask
    map CA.stringify <$> mkRequest { baseUrl, endpoint: BibleAPI, method: Get }

instance manageBibleAppM :: ManageBible AppM where
  downloadBible version = do
    { bibleApiUrl } <- ask
    let conf = { baseUrl: bibleApiUrl, endpoint: DownloadBible version, method: Get }
    decode_ =<< mkRequest conf
