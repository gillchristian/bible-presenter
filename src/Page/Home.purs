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
import Web.UIEvent.MouseEvent (toEvent)

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
    Layout.dashboard Nothing Navigate (Just Home) content
    where
    content =
      HH.div
        [ HP.classes
            [ T.maxW2xl
            , T.mxAuto
            , T.textCenter
            , T.py16
            , T.px4
            , T.smPy20
            , T.smPx6
            , T.lgPx8
            ]
        ]
        [ HH.h2
            [ HP.classes [ T.text3xl, T.fontExtrabold, T.textIndigo600, T.smText4xl ] ]
            [ HH.span
                [ HP.classes [ T.block ] ]
                [ HH.text "Present Bible verses on the fly." ]
            ]
        , HH.p
            [ HP.classes [ T.mt4, T.textLg, T.leading6, T.textGray600 ] ]
            [ HH.text "Ac euismod vel sit maecenas id pellentesque eu sed consectetur. Malesuada adipiscing sagittis vel nulla nec." ]
        , HH.a
            [ HP.classes
                [ T.mt8
                , T.wFull
                , T.inlineFlex
                , T.itemsCenter
                , T.justifyCenter
                , T.px5
                , T.py3
                , T.border2
                , T.borderTransparent
                , T.textBase
                , T.fontMedium
                , T.roundedMd
                , T.textWhite
                , T.bgIndigo600
                , T.hoverBgWhite
                , T.hoverTextIndigo600
                , T.hoverBorderIndigo600
                , T.smWAuto
                ]
            , safeHref Dashboard
            , HE.onClick $ Just <<< Navigate Dashboard <<< toEvent
            ]
            [ HH.text "Start presenting" ]
        ]
