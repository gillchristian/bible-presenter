## Notes

- `preventDefault` commit: https://github.com/thomashoneyman/purescript-halogen-realworld/commit/5ccdde8efe3cc9d32a1077e7c2fe66185161905a
- Paralelize `AppM`: https://github.com/thomashoneyman/purescript-halogen-realworld/commit/4db3e750773b07bdd97fd908b3fd2de662b84222
- Update codecs: https://github.com/thomashoneyman/purescript-halogen-realworld/commit/592eab66544b8768153ea2c415de58d7445d9267
- Skeletons
  - https://codepen.io/havardob/pen/dyGGGzq
  - [Tailwind](https://www.youtube.com/watch?v=_OZYvKsn60g)

- Google Auth: https://github.com/FrigoEU/purescript-google-auth

## Components

**Toggle Switch**

```purescript
toggle action disabled on =
  HH.button
    [ HP.type_ HP.ButtonButton
    , HE.onClick \_ -> action
    , HP.disabled disabled
    , HP.classes
        [ cx T.bgGray200 $ not on
        , cx T.bgKiwi on
        , T.relative
        , T.inlineFlex
        , T.flexShrink0
        , T.h6
        , T.w11
        , T.border2
        , T.borderTransparent
        , T.roundedFull
        , T.cursorPointer
        , T.transitionColors
        , T.easeInOut
        , T.duration200
        , T.focusOutlineNone
        , T.focusRing2
        , T.focusRingOffset2
        , T.focusRingKiwi
        , T.disabledCursorNotAllowed
        , T.disabledOpacity50
        ]
    ]
    [ HH.span [ HP.classes [ T.srOnly ] ] [ HH.text "Use setting" ]
    , HH.span
        [ HP.classes
            [ cx T.translateX0 $ not on
            , cx T.translateX5 on
            , T.inlineBlock
            , T.h5
            , T.w5
            , T.roundedFull
            , T.bgWhite
            , T.shadow
            , T.transform
            , T.ring0
            , T.transition
            , T.easeInOut
            , T.duration200
            ]
        ]
        []
    ]
```

**Button Group**

```purescript
HH.span
  [ HP.classes [ T.relative, T.z0, T.inlineFlex, T.roundedMd ] ]
  [ HH.button
      [ HE.onClick \_ -> Just $ CompleteResource next
      , HP.type_ HP.ButtonButton
      , HP.classes
          [ T.relative
          , T.inlineFlex
          , T.itemsCenter
          , T.px4
          , T.py1
          , T.roundedLMd
          , T.border
          , T.borderGray300
          , T.bgWhite
          , T.textGray400
          , T.textXs
          , T.fontMedium
          , T.textWhite
          , T.hoverBgGray100
          , T.focusZ10
          , T.focusOutlineNone
          , T.focusRing1
          , T.focusRingKiwi
          , T.focusBorderKiwi
          ]
      ]
      [ HH.text "✔️" ]
  , HH.button
      [ HE.onClick \_ -> Just $ DeleteResource next
      , HP.type_ HP.ButtonButton
      , HP.classes
          [ T.negMlPx
          , T.inlineFlex
          , T.itemsCenter
          , T.px4
          , T.py1
          , T.roundedRMd
          , T.border
          , T.borderGray300
          , T.bgWhite
          , T.textGray400
          , T.textXs
          , T.fontMedium
          , T.textWhite
          , T.hoverBgGray100
          , T.focusZ10
          , T.focusOutlineNone
          , T.focusRing1
          , T.focusRingKiwi
          , T.focusBorderKiwi
          ]
      ]
      [ HH.text "❌" ]
  ]
```
