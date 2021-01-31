module Bible.AppM where

import Prelude

import Bible.Api.Endpoint (Endpoint(..))
import Bible.Api.Request (RequestMethod(..))
import Bible.Api.Utils (mkRequest)
import Bible.Capability.Clipboard (class Clipboard)
import Bible.Capability.LogMessages (class LogMessages)
import Bible.Capability.Navigate (class Navigate, locationState, navigate)
import Bible.Capability.Now (class Now)
import Bible.Capability.Resource.List (class ManageList)
import Bible.Data.Log as Log
import Bible.Data.Route as Route
import Bible.Env (Env, LogLevel(..))
import Bible.Foreign.Clipboard as ForeignClipboard
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Argonaut.Core as CA
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Routing.Duplex (print)
import Type.Equality (class TypeEquals, from)
import Web.Event.Event (preventDefault)

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
  createList _ =
    map CA.stringify <$> mkRequest { endpoint: BibleAPI, method: Get }
