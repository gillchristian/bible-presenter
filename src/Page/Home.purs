module Bible.Page.Home where

import Prelude

import Bible.Capability.Navigate (class Navigate, navigate_)
import Bible.Component.HTML.Layout as Layout
import Bible.Component.HTML.Utils (safeHref)
import Bible.Data.Route (Route(..))
import Bible.Env (UserEnv)
import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Tailwind as T
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent as MouseEvent

data Action
  = Initialize
  | Receive { currentUser :: Maybe Boolean }
  | Navigate Route Event

type State = {currentUser :: Maybe Boolean}

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
  initialState { currentUser } = { currentUser }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> pure unit

    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

    Navigate route e -> navigate_ e route

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { currentUser } =
    Layout.dashboard
      currentUser
      Navigate
      (Just Home)
      $ HH.div
          []
          [ HH.h1
              [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
              [ HH.text "Easily present Bible verses" ]
          , HH.p
              [ HP.classes [ T.textGray400, T.textLg ] ]
              [ HH.text "Keep your reading and watching lists under control with Listas" ]
          , HH.p
              [ HP.classes [ T.mt6 ] ]
              [ HH.a
                  [ safeHref Dashboard
                  , HE.onClick \e -> Just $ Navigate Dashboard $ MouseEvent.toEvent e
                  , HP.classes
                      [ T.textGray600, T.textLg, T.hoverUnderline ]
                  ]
                  [ HH.text "Go to Dashboard" ]
              ]
          , HH.p
              [ HP.classes [ T.mt6 ] ]
              [ HH.a
                  [ safeHref Dashboard
                  , HE.onClick \e -> Just $ Navigate Presenter $ MouseEvent.toEvent e
                  , HP.classes
                      [ T.textGray600, T.textLg, T.hoverUnderline ]
                  ]
                  [ HH.text "Go to Presenter" ]
              ]
          ]
