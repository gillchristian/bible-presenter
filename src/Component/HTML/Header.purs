module Bible.Component.HTML.Header where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Bible.Component.HTML.Utils (safeHref)
import Bible.Data.Route (Route(..))
import Tailwind as T
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent (toEvent)

header :: forall i p r. Maybe Boolean -> (Route -> Event -> p) -> Maybe Route -> HH.HTML i p
header _ navigate route =
  HH.nav
    [ HP.classes [ T.py12, T.container, T.flex, T.justifyBetween, T.itemsCenter, T.flexWrap ] ]
    [ HH.a
        [ HP.classes [ T.border2, T.py3, T.px2, T.borderGray400 ]
        , safeHref Home
        , HE.onClick (onNavigate Home)
        ]
        [ HH.h1
            [ HP.classes [ T.text2xl, T.leadingNone, T.textGray400 ] ]
            [ HH.text "Bible Presenter" ]
        ]
    ]
  where
  onNavigate r = Just <<< navigate r <<< toEvent
