module Bible.Component.HTML.Layout where

import Prelude

import Bible.Component.HTML.Icons as Icons
import Bible.Component.HTML.Utils (cx, maybeElem, safeHref)
import Bible.Data.Route (Route(..))
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Tailwind as T
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent (toEvent)

dashboard :: forall i p. Maybe String -> (Route -> Event -> p) -> Maybe Route -> HH.HTML i p -> HH.HTML i p
dashboard mbTitle navigate activeRoute content =
  HH.div
    [ HP.classes [ T.minHScreen, T.bgGray100 ] ]
    [ HH.div
        [ HP.classes [ T.bgIndigo600, T.pb32 ] ]
        [ nav navigate activeRoute
        , HH.header
            [ HP.classes [ T.py10 ] ]
            [ HH.div
                [ HP.classes [ T.maxW7xl, T.mxAuto, T.px4, T.smPx6, T.lgPx8 ] ]
                [ maybeElem mbTitle \title ->
                    HH.h1
                      [ HP.classes [ T.text3xl, T.fontBold, T.textWhite ] ]
                      [ HH.text title ]
                ]
            ]
        ]
    , HH.main
        [ HP.classes [ T.negMt32 ] ]
        [ HH.div
            [ HP.classes
                [ T.mxAuto
                , T.pb12
                , T.px4
                , T.smPx6
                , T.lgPx8
                ]
            ]
            [ HH.div
                [ HP.classes
                    [ T.bgWhite
                    , T.roundedLg
                    , T.shadow
                    , T.px5
                    , T.py6
                    , T.smPx6
                    ]
                ]
                [ content ]
            ]
        ]
    ]
  where
  onNavigate r = Just <<< navigate r <<< toEvent

nav :: forall i p. (Route -> Event -> p) -> Maybe Route -> HH.HTML i p
nav navigate activeRoute =
  HH.nav
    [ HP.classes [ T.bgIndigo600, T.borderB, T.borderIndigo300, T.borderOpacity25, T.lgBorderNone ] ]
    [ HH.div
        [ HP.classes [ T.maxW7xl, T.mxAuto, T.px2, T.smPx4, T.lgPx8 ] ]
        [ HH.div
            [ HP.classes
                [ T.relative
                , T.h16
                , T.flex
                , T.itemsCenter
                , T.justifyBetween
                , T.lgBorderB
                , T.lgBorderIndigo400
                , T.lgBorderOpacity25
                ]
            ]
            [ HH.div
                [ HP.classes [ T.px2, T.flex, T.itemsCenter, T.lgPx0 ] ]
                [ HH.div
                    [ HP.classes [ T.flexShrink0 ] ]
                    [ HH.a
                        [ safeHref Home
                        , HE.onClick $ onNavigate Home
                        , HP.classes [ T.cursorPointer ]
                        ]
                        [ Icons.openBook [ Icons.classes [ T.block, T.h8, T.w8, T.textWhite ] ] ]
                    ]
                , HH.div
                    [ HP.classes [ T.hidden, T.lgBlock, T.lgMl10 ] ]
                    [ HH.div
                        [ HP.classes [ T.flex, T.spaceX4 ] ]
                        [ desktopLink "Dashboard" Dashboard
                        , desktopLink "Presentation" Presenter
                        ]
                    ]
                ]
            ]
        ]
    , HH.div
        [ HP.classes [ T.lgHidden ] ]
        [ HH.div
            [ HP.classes [ T.px2, T.pt2, T.pb3, T.spaceY1 ] ]
            [ mobileLink "Dashboard" Dashboard
            , mobileLink "Presentation" Presenter
            ]
        ]
    ]
  where
  onNavigate r = Just <<< navigate r <<< toEvent

  desktopLink label route =
    HH.a
      [ HP.classes
          [ cx T.bgIndigo700 active
          , cx T.hoverBgIndigo500 $ not active
          , cx T.hoverBgOpacity75 $ not active
          , T.textWhite
          , T.roundedMd
          , T.py2
          , T.px3
          , T.textSm
          , T.fontMedium
          ]
      , safeHref route
      , HE.onClick $ onNavigate route
      ]
      [ HH.text label ]
    where
    active = activeRoute == Just route

  mobileLink label route =
    HH.a
      [ HP.classes
          [ cx T.bgIndigo700 active
          , cx T.hoverBgIndigo500 $ not active
          , cx T.hoverBgOpacity75 $ not active
          , T.textWhite
          , T.block
          , T.roundedMd
          , T.py2
          , T.px3
          , T.textBase
          , T.fontMedium
          ]
      , safeHref route
      , HE.onClick $ onNavigate route
      ]
      [ HH.text label ]
    where
    active = activeRoute == Just route
