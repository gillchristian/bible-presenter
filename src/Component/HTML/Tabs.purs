module Bible.Component.HTML.Tabs where

import Prelude

import Bible.Component.HTML.Utils (cx)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Maybe (Maybe)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Tailwind as T

type Tab action
  = { active :: Boolean
    , label :: String
    , action :: Maybe action
    }

tabs :: forall i p. NonEmptyArray (Tab p) -> HH.HTML i p
tabs ts =
  HH.div
    [ HP.classes [ T.borderB, T.borderGray200 ] ]
    [ HH.nav
        [ HP.classes [ T.negMbPx, T.flex, T.justifyBetween ] ]
        $ map tab
        $ toArray ts
    ]

tab :: forall i p. Tab p -> HH.HTML i p
tab { active, action, label } =
  HH.a
    [ HE.onClick \_ -> action
    , HP.classes
        [ cx T.borderTransparent $ not active
        , cx T.textGray500 $ not active
        , cx T.hoverTextGray700 $ not active
        , cx T.hoverBorderGray300 $ not active
        , cx T.borderIndigo500 active
        , cx T.textIndigo600 active
        , T.flexGrow
        , T.py4
        , T.px1
        , T.textCenter
        , T.borderB2
        , T.fontMedium
        , T.textSm
        , T.cursorPointer
        ]
    ]
    [ HH.text label ]
