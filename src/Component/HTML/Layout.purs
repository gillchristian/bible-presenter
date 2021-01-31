module Bible.Component.HTML.Layout where

import Data.Maybe (Maybe)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Bible.Component.HTML.Header (header)
import Bible.Data.Route (Route)
import Tailwind as T
import Web.Event.Event (Event)

dashboard :: forall i p.
     Maybe Boolean
  -> (Route -> Event -> p)
  -> Maybe Route
  -> HH.HTML i p
  -> HH.HTML i p
dashboard currentUser navigate route content =
  HH.div
    [ HP.classes [ T.minHScreen, T.wScreen, T.bgGray100 ] ]
    [ HH.div
        [ HP.classes [ T.container, T.mxAuto, T.px2, T.pb20 ] ]
        [ header currentUser navigate route
        , content
        ]
    ]
