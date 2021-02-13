module Bible.Component.HTML.Breadcrumbs where

import Prelude

import Bible.Component.HTML.Icons as Icons
import Bible.Component.HTML.Utils (cx)
import Data.Array (cons, null)
import Data.Foldable (length)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Tailwind as T

type Crumb action
  = { label :: String
    , action :: Maybe action
    }

breadcrumbs :: forall i p. Icons.Icon -> Crumb p -> Array (Crumb p) -> HH.HTML i p
breadcrumbs mainIcon main rest =
  HH.nav
    [ HP.classes [ T.flex ] ]
    [ HH.ol
        [ HP.classes [ T.flex, T.itemsCenter, T.spaceX4 ] ]
        $ cons mainEl
        $ mapWithIndex (crumb $ length rest) rest

    ]
  where
  mainEl =
    HH.li
      []
      [ HH.a
          [ HE.onClick \_ -> main.action
          , HP.classes
              [ T.flex
              , T.itemsCenter
              , T.textGray500
              , T.hoverTextGray700
              , cx T.cursorPointer $ not $ null rest
              ]
          ]
          [ HH.div
              []
              [ mainIcon [ Icons.classes [ T.flexShrink0, T.h5, T.w5 ] ] ]
          , HH.span
              [ HP.classes [ T.ml4, T.textSm, T.fontMedium ] ]
              [ HH.text main.label ]
          ]
      ]

crumb :: forall i p. Int -> Int -> Crumb p -> HH.HTML i p
crumb total i {label, action} =
  HH.li
    []
    [ HH.div
        [ HP.classes [ T.flex, T.itemsCenter ] ]
        [ Icons.chevronRight [ Icons.classes [ T.flexShrink0, T.h5, T.w5, T.textGray400 ] ]
        , HH.a
            [ HE.onClick \_ -> action
            , HP.classes
                [ T.ml4
                , T.textSm
                , T.fontMedium
                , T.textGray500
                , T.hoverTextGray700
                , cx T.cursorPointer $ not isLast
                ]
            ]
            [ HH.text label ]
        ]
    ]
  where isLast = total == i + 1
