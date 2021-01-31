-- | The `Router` component is the root of our Halogen application. Every other component is a
-- | direct descendent of this component. We'll use the router to choose which component to render
-- | given a particular `Route` and to manage the user's location in the application.
module Bible.Component.Router where

import Prelude

import Bible.Capability.Clipboard (class Clipboard)
import Bible.Capability.LogMessages (class LogMessages)
import Bible.Capability.Navigate (class Navigate, navigate, locationState)
import Bible.Capability.Now (class Now)
import Bible.Component.Utils (OpaqueSlot)
import Bible.Data.Route (Route(..), routeCodec)
import Bible.Env (UserEnv)
import Bible.Page.Dashboard as Dashboard
import Bible.Page.Home as Home
import Bible.Page.Presenter as Presenter
import Component.HOC.Connect (WithCurrentUser)
import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Routing.Duplex as RD

type State =
  { route :: Maybe Route
  , currentUser :: Maybe Boolean
  }

data Query a
  = Navigate Route a

data Action
  = Initialize
  | Receive { | WithCurrentUser () }

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , dashboard :: OpaqueSlot Unit
  , presenter :: OpaqueSlot Unit
  )

component
  :: forall m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => Now m
  => LogMessages m
  => Navigate m
  => Clipboard m
  => H.Component HH.HTML Query {} Void m
component = Connect.component $ H.mkComponent
  { initialState: \{ currentUser } -> { route: Nothing, currentUser }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      -- first we'll get the route the user landed on
      initialRoute <- hush <<< (RD.parse routeCodec) <$> _.path <$> locationState
      -- then we'll navigate to the new route (also setting the hash)
      navigate $ fromMaybe Home initialRoute

    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route, currentUser } <- H.get
      when (route /= Just dest) $ H.modify_ _ { route = Just dest }
      pure $ Just a

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route, currentUser } = case route of
    Just r -> case r of
      Home ->
        HH.slot (SProxy :: _ "home") unit Home.component {} absurd

      Dashboard ->
        HH.slot (SProxy :: _ "dashboard") unit Dashboard.component {} absurd

      Presenter ->
        HH.slot (SProxy :: _ "presenter") unit Presenter.component {} absurd

    Nothing ->
      HH.div_ [ HH.text "Oh no! That page wasn't found." ]
