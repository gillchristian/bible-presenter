module Bible.Component.HTML.Slide where

import Prelude

import Bible.Data.Slide (Slide, SlideContent(..))
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Tailwind as T

imgA :: String
imgA = "https://images.unsplash.com/photo-1506744038136-46273834b3fb?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1950&q=80"

highlight :: forall i p. String -> HH.HTML i p
highlight text =
  HH.span
    [ HP.classes [ T.textGray900, T.bgWhite, T.bgOpacity75, T.leadingNormal ] ]
    [ HH.text text ]

multiline :: forall i p. String -> Array (HH.HTML i p)
multiline  =
   map (\s -> HH.p [] [ highlight s ]) <<< split (Pattern "\n")

slide :: forall i p. Slide -> HH.HTML i p
slide { background, content } =
    HH.div
      [ HP.classes
          [ T.hScreen
          , T.wScreen
          , T.py20
          , T.px40
          , T.bgGray100
          , T.flex
          , T.justifyCenter
          , T.itemsCenter
          , T.bgCenter
          , T.bgCover
          ]
      , HP.prop (H.PropName "style")
          $ "background-image: url('" <> fromMaybe imgA background <> "');"
      ]
      [ case content of
          Still -> HH.text ""
          Text text ->
            HH.div
              [ HP.classes [ T.text7xl, T.textWhite, T.fontExtrabold ] ]
              $ multiline text
          Verse { book, chapter, verse, contents } ->
            HH.div
              [ HP.classes [ T.text5xl, T.fontExtrabold ] ]
              [ HH.p
                  [ HP.classes [ T.text5xl, T.mb10 ] ]
                  [ highlight contents ]
              , HH.p
                  [ HP.classes [ T.text3xl ] ]
                  [ highlight $ book <> " " <> show chapter <> ":" <> show verse ]
              ]
      ]
