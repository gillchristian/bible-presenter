module Bible.Page.Dashboard where

import Prelude

import Bible.Capability.Navigate (class Navigate, navigate_)
import Bible.Component.HTML.Layout as Layout
import Bible.Component.HTML.Utils (cx)
import Bible.Data.Presentation (ToCoordinator(..), ToPresenter(..))
import Bible.Data.Presentation as Presentation
import Bible.Data.Route (Route(..))
import Bible.Env (UserEnv)
import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Natural (Natural, intToNat, natToInt)
import Data.Slider (Slider(..))
import Data.Slider as Slider
import Data.String as String
import Data.ZipperArray as ZipperArray
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Tailwind as T
import Web.Event.Event (Event)
import Web.UIEvent.KeyboardEvent as KE

data Action
  = Initialize
  | Receive { currentUser :: Maybe Boolean }
  | Navigate Route Event
  | NewSlideContentChange String
  | AddSlide
  | Start
  | Next
  | Prev
  | GoToSlide Natural
  | ImgSelected String
  | ReceiveMsg ToCoordinator
  | SendMsg ToPresenter

type Slide
  = { contents :: String }

type State
  = { currentUser :: Maybe Boolean
    , slides :: Slider Slide
    , newSlide :: String
    , bgImage :: String
    , channel :: Maybe (Presentation.Channel ToPresenter)
    }

imgA :: String
imgA = "https://images.unsplash.com/photo-1506744038136-46273834b3fb?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1950&q=80"

imgs :: Array String
imgs =
  [ imgA
  , "https://images.unsplash.com/photo-1611030225755-3082ed97e11e?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1951&q=80"
  , "https://images.unsplash.com/photo-1611569689188-98d6173842c1?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1950&q=80"
  ]

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
    , slides: Inactive []
    , newSlide: ""
    , bgImage: imgA
    , channel: Nothing
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      { source, channel } <- H.liftEffect $ Presentation.mkChannel "channel" ReceiveMsg
      void $ H.subscribe source
      H.modify_ _ { channel = Just channel }

    ReceiveMsg GetState -> do
      { bgImage } <- H.get
      handleAction $ SendMsg $ PassState bgImage

    SendMsg msg -> do
      { channel } <- H.get
      case channel of
        Just { postMessage } ->  H.liftEffect $ postMessage msg
        Nothing -> pure unit

    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

    Navigate route e -> navigate_ e route

    AddSlide -> do
      {newSlide} <- H.get
      case newSlide of
        "" -> pure unit
        contents ->
          H.modify_ $ \s -> s { newSlide = "", slides = Slider.snoc s.slides { contents } }

    NewSlideContentChange contents ->
      H.modify_ _ { newSlide = contents }

    ImgSelected img -> do
      H.modify_ _ { bgImage = img }
      handleAction $ SendMsg $ SetImage img

    Start -> do
      {slides} <- H.get
      case slides of
        Active _ -> pure unit
        Inactive as ->
          case ZipperArray.fromArray as of
            Nothing -> pure unit
            Just asZip -> do
               H.modify_ _ { slides = Active asZip }
               handleAction $ SendMsg $ SetContent $ _.contents $ ZipperArray.current asZip

    GoToSlide n -> do
      {slides} <- H.get
      case slides of
        -- TODO: switch to active
        Inactive _ -> pure unit
        Active as ->
          case ZipperArray.goIndex n as of
            Nothing -> pure unit
            Just updated -> do
              handleAction $ SendMsg $ SetContent $ _.contents $ ZipperArray.current updated
              H.modify_ _ { slides = Active updated }

    Prev -> do
      {slides} <- H.get
      case slides of
        Inactive _ -> pure unit
        Active as ->
          case ZipperArray.goPrev as of
            Nothing -> pure unit
            Just updated -> do
              handleAction $ SendMsg $ SetContent $ _.contents $ ZipperArray.current updated
              H.modify_ _ { slides = Active updated }

    Next -> do
      {slides} <- H.get
      case slides of
        Inactive _ -> pure unit
        Active as ->
          case ZipperArray.goNext as of
            Nothing -> pure unit
            Just updated -> do
              handleAction $ SendMsg $ SetContent $ _.contents $ ZipperArray.current updated
              H.modify_ _ { slides = Active updated }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { currentUser, newSlide, slides, bgImage } =
    Layout.dashboard currentUser Navigate (Just Dashboard) content
    where

    newSlideEl "" = HH.text ""
    newSlideEl contents =
      HH.div
        [ HP.classes
            [ T.bgGray200
            , T.textGray500
            , T.flex
            , T.justifyCenter
            , T.itemsCenter
            , T.py12
            , T.px4
            , T.roundedMd
            ]
        ]
        [ HH.text contents ]

    curSlide :: Int -> Slide -> _
    curSlide _ { contents } =
      HH.div
        [ HP.classes
            [ T.bgGray300
            , T.textGray700
            , T.flex
            , T.justifyCenter
            , T.itemsCenter
            , T.py12
            , T.px4
            , T.roundedMd
            , T.ring2
            , T.ringOffset2
            , T.ringIndigo500
            , T.hoverRing2
            , T.hoverRingOffset2
            , T.hoverRingGreen400
            , T.selectNone
            ]
        ]
        [ HH.text contents ]

    restSlide :: Natural -> Int -> Slide -> _
    restSlide offset i { contents } =
      HH.div
        [ HE.onDoubleClick $ \_ -> Just $ GoToSlide $ offset + intToNat i
        , HP.classes
            [ T.bgGray300
            , T.textGray700
            , T.flex
            , T.justifyCenter
            , T.itemsCenter
            , T.py12
            , T.px4
            , T.roundedMd
            , T.hoverRing2
            , T.hoverRingOffset2
            , T.hoverRingGreen400
            , T.selectNone
            ]
        ]
        [ HH.text contents ]

    imgEl img =
      HH.img
        [ HP.classes
            [ T.roundedMd
            , T.cursorPointer
            , T.hoverRing2
            , T.hoverRingGreen400
            , T.hoverRingOffset2
            , cx T.ring2 $ bgImage == img
            , cx T.ringOffset2 $ bgImage == img
            , cx T.ringIndigo500 $ bgImage == img
            ]
        , HP.src img
        , HE.onClick \_ -> Just $ ImgSelected img
        ]

    content =
      HH.div
        [ HP.classes [ T.grid, T.gridCols3, T.gap4 ] ]
        [ HH.div
            [ HP.classes [ T.colSpan2 ] ]
            [ HH.div
                []
                [ HH.label
                    [ HP.classes [ T.textGray700, T.textLg ] ]
                    [ HH.text "Slide content" ]
                , HH.textarea
                    [ HP.value newSlide
                    , HE.onKeyDown \e ->
                        case KE.key e of
                          "Enter" | KE.ctrlKey e || KE.metaKey e -> Just AddSlide
                          _ -> Nothing
                    , HE.onValueInput $ Just <<< NewSlideContentChange
                    , HP.classes
                        [ T.appearanceNone
                        , T.borderNone
                        , T.wFull
                        , T.my2
                        , T.py2
                        , T.px4
                        , T.bgGray200
                        , T.textGray700
                        , T.placeholderGray400
                        , T.roundedMd
                        , T.shadowMd
                        , T.textBase
                        , T.focusBgWhite
                        , T.focusOutlineNone
                        , T.focusRing2
                        , T.focusRingOffset2
                        , T.focusRingGreen400
                        ]
                    , HP.rows 5
                    , HP.placeholder "Something something interesting"
                    ]
                , HH.button
                    [ HP.type_ HP.ButtonButton
                    , HE.onClick \_ -> Just AddSlide
                    , HP.classes
                        [ T.flex1
                        , T.wFull
                        , T.cursorPointer
                        , T.disabledCursorNotAllowed
                        , T.disabledBgGray300
                        , T.py2
                        , T.px4
                        , T.bgGreen400
                        , T.hoverBgGreen600
                        , T.textWhite
                        , T.roundedMd
                        , T.shadowMd
                        , T.focusOutlineNone
                        , T.focusRing2
                        , T.focusRingOffset2
                        , T.focusRingGreen400
                        ]
                    , HP.disabled $ String.null newSlide
                    ]
                    [ HH.text "Add Slide" ]
                , HH.div
                    [ HP.classes [ T.textGray700, T.textLg, T.mt8 ] ]
                    [ HH.text "Set background" ]
                , HH.div
                    [ HP.classes [ T.grid, T.gridCols3, T.gap4, T.mt2 ] ]
                    $ map imgEl imgs
                ]
            ]
        , HH.div
            []
            [ HH.div
                [ HP.classes [ T.textGray700, T.textLg, T.mb2 ] ]
                [ HH.text "Slides queue" ]
            , case slides of
                Active as ->
                  HH.div
                    [ HP.classes [ T.grid, T.gridCols2, T.gap4 ] ]
                    [ HH.button
                        [ HP.type_ HP.ButtonButton
                        , HE.onClick \_ -> Just Prev
                        , HP.classes
                            [ T.flex1
                            , T.wFull
                            , T.cursorPointer
                            , T.disabledCursorNotAllowed
                            , T.disabledBgGray300
                            , T.py2
                            , T.px4
                            , T.mb2
                            , T.bgGreen400
                            , T.hoverBgGreen600
                            , T.textWhite
                            , T.roundedMd
                            , T.shadowMd
                            , T.focusOutlineNone
                            , T.focusRing2
                            , T.focusRingOffset2
                            , T.focusRingGreen400
                            ]
                        , HP.disabled $ ZipperArray.atStart as
                        ]
                        [ HH.text "Prev" ]
                    , HH.button
                        [ HP.type_ HP.ButtonButton
                        , HE.onClick \_ -> Just Next
                        , HP.classes
                            [ T.flex1
                            , T.wFull
                            , T.cursorPointer
                            , T.disabledCursorNotAllowed
                            , T.disabledBgGray300
                            , T.py2
                            , T.px4
                            , T.mb2
                            , T.bgGreen400
                            , T.hoverBgGreen600
                            , T.textWhite
                            , T.roundedMd
                            , T.shadowMd
                            , T.focusOutlineNone
                            , T.focusRing2
                            , T.focusRingOffset2
                            , T.focusRingGreen400
                            ]
                        , HP.disabled $ ZipperArray.atEnd as
                        ]
                        [ HH.text "Next" ]
                    ]
                Inactive as ->
                  HH.button
                    [ HP.type_ HP.ButtonButton
                    , HE.onClick \_ -> Just Start
                    , HP.classes
                        [ T.flex1
                        , T.wFull
                        , T.cursorPointer
                        , T.disabledCursorNotAllowed
                        , T.disabledBgGray300
                        , T.py2
                        , T.px4
                        , T.mb2
                        , T.bgGreen400
                        , T.hoverBgGreen600
                        , T.textWhite
                        , T.roundedMd
                        , T.shadowMd
                        , T.focusOutlineNone
                        , T.focusRing2
                        , T.focusRingOffset2
                        , T.focusRingGreen400
                        ]
                    , HP.disabled $ Array.null as
                    ]
                    [ HH.text "Start" ]
            , case slides of
                Active slides' ->
                  HH.div
                    [ HP.classes [ T.mt6, T.flex, T.flexCol, T.gap4 ] ]
                    $ Array.cons (curSlide (natToInt $ ZipperArray.curIndex slides') (ZipperArray.current slides'))
                    $ flip Array.snoc (newSlideEl newSlide)
                    $ mapWithIndex (restSlide $ ZipperArray.curIndex slides' + intToNat 1)
                    $ ZipperArray.succ slides'
                Inactive as ->
                  HH.div
                    [ HP.classes [ T.mt6, T.flex, T.flexCol, T.gap4 ] ]
                    $ flip Array.snoc (newSlideEl newSlide)
                    $ mapWithIndex (restSlide $ intToNat 0) as
            ]
        ]
