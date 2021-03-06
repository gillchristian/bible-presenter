module Bible.Page.Presenter where

import Prelude

import Bible.Capability.Navigate (class Navigate, navigate_)
import Bible.Component.HTML.Icons as Icons
import Bible.Component.HTML.Slide as Slide
import Bible.Component.HTML.Utils (whenElem)
import Bible.Data.Presentation (ToCoordinator(..), ToPresenter(..))
import Bible.Data.Presentation as Presentation
import Bible.Data.Route (Route)
import Bible.Data.Slide (Slide, SlideContent(..))
import Bible.Env (UserEnv)
import Bible.Foreign.Fullscreen (checkFullScreen, exitFullScreen, requestFullScreen)
import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as HES
import Tailwind as T
import Web.Event.Event (Event, EventType(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument as Doc
import Web.HTML.Window (document)

data Action
  = Initialize
  | Receive { currentUser :: Maybe Boolean }
  | Navigate Route Event
  | ReceiveMsg ToPresenter
  | SendMsg ToCoordinator
  | SetFullScreen Boolean
  | ToggleFullScreen

type State
  = { currentUser :: Maybe Boolean
    , channel :: Maybe (Presentation.Channel ToCoordinator)
    , content :: Maybe Slide
    , fullScreen :: Boolean
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
    , content: Nothing
    , fullScreen: false
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      { source, channel } <- H.liftEffect $ Presentation.mkChannel "channel" ReceiveMsg
      void $ H.subscribe source
      H.liftEffect $ channel.postMessage GetState
      H.modify_ _ { channel = Just channel }

      doc <- H.liftEffect $ (document =<< window)

      void $ H.subscribe $ HES.eventListenerEventSource (EventType "fullscreenchange") (Doc.toEventTarget doc) \_ ->
        Just $ SetFullScreen $ checkFullScreen $ Doc.toDocument doc

    ReceiveMsg (SetSlide content) ->
      H.modify_ _ { content = Just content }

    SendMsg msg -> do
      { channel } <- H.get
      case channel of
        Just { postMessage } -> H.liftEffect $ postMessage msg
        Nothing -> pure unit

    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

    SetFullScreen fullScreen ->
      H.modify_ _ { fullScreen = fullScreen }

    ToggleFullScreen -> do
      {fullScreen} <- H.get
      doc <- H.liftEffect $ document =<< window

      when fullScreen do
        void $ H.liftAff $ exitFullScreen $ Doc.toDocument doc

      unless fullScreen do
        mbBody <- H.liftEffect $ Doc.body doc
        for_ mbBody \body -> H.liftAff $ requestFullScreen body

    Navigate route e -> navigate_ e route

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { currentUser, content, fullScreen } =
    HH.div
      [ HE.onDoubleClick \_ -> Just ToggleFullScreen ]
      [ maybe fallback Slide.slide content
      , HH.div
          [ HP.classes
              [ T.fixed
              , T.z10
              , T.bottom4
              , T.right4
              , T.cursorPointer
              , T.p1
              , T.bgGray200
              , T.bgOpacity50
              , T.roundedMd
              ]
          , HE.onClick \_ -> Just ToggleFullScreen
          ]
          [ whenElem (not fullScreen) \_ -> Icons.maximize [ Icons.classes [ T.h8, T.w8, T.textGray700 ] ]
          , whenElem fullScreen \_ -> Icons.minimize [ Icons.classes [ T.h8, T.w8, T.textGray700 ] ]
          ]
      ]
    where
    fallback = Slide.slide { background: Just defaultImage, content: Still }
