module Bible.Page.Presenter where

import Prelude

import Bible.Capability.Navigate (class Navigate, navigate_)
import Bible.Component.HTML.Utils (maybeElem)
import Bible.Data.Presentation (ToCoordinator(..), ToPresenter(..))
import Bible.Data.Presentation as Presentation
import Bible.Data.Route (Route)
import Bible.Env (UserEnv)
import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Tailwind as T
import Web.Event.Event (Event)

data Action
  = Initialize
  | Receive { currentUser :: Maybe Boolean }
  | Navigate Route Event
  | ReceiveMsg ToPresenter
  | SendMsg ToCoordinator

type State
  = { currentUser :: Maybe Boolean
    , channel :: Maybe (Presentation.Channel ToCoordinator)
    , bgImage :: String
    , content :: Maybe String
    }

defaultImage :: String
defaultImage = "https://images.unsplash.com/photo-1506744038136-46273834b3fb?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1950&q=80"

component
  :: forall q o m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => Navigate m
  => H.Component HH.HTML q {} o m
component = Connect.component $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  initialState { currentUser } =
    { currentUser
    , channel: Nothing
    , bgImage: defaultImage
    , content: Nothing
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      { source, channel } <- H.liftEffect $ Presentation.mkChannel "channel" ReceiveMsg
      void $ H.subscribe source
      handleAction $ SendMsg $ GetState
      H.modify_ _ { channel = Just channel }

    ReceiveMsg (SetImage img) -> H.modify_ _ { bgImage = img }

    ReceiveMsg (PassState img) -> H.modify_ _ { bgImage = img }

    ReceiveMsg (SetContent content) -> H.modify_ _ { content = Just content }

    SendMsg msg -> do
      { channel } <- H.get
      case channel of
        Just { postMessage } ->  H.liftEffect $ postMessage msg
        Nothing -> pure unit

    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

    Navigate route e -> navigate_ e route

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { currentUser, bgImage, content: mbContent } =
    HH.div
      [ HP.classes
          [ T.hScreen
          , T.wScreen
          , T.bgGray100
          , T.flex
          , T.justifyCenter
          , T.itemsCenter
          , T.bgCenter
          , T.bgCover
          ]
      , HP.prop (H.PropName "style") $ "background-image: url('" <> bgImage <> "');"
      ]
      [ maybeElem mbContent \content ->
          HH.div
            [ HP.classes [ T.text7xl, T.textWhite, T.fontExtrabold ] ]
            [ HH.text content ]

      ]
