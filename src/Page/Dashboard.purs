module Bible.Page.Dashboard where

import Prelude

import Bible.Capability.LocalStorage (class LocalStorage, getItem, removeItem, setItem)
import Bible.Capability.Navigate (class Navigate, navigate_)
import Bible.Capability.Resource.Bible (class ManageBible, downloadBible)
import Bible.Component.HTML.Layout as Layout
import Bible.Component.HTML.Slide as Slide
import Bible.Component.HTML.Utils (cx)
import Bible.Data.Bible (Bible)
import Bible.Data.Bible as Bible
import Bible.Data.Presentation (ToCoordinator(..), ToPresenter(..))
import Bible.Data.Presentation as Presentation
import Bible.Data.Route (Route(..))
import Bible.Data.Slide (Slide, SlideContent(..))
import Bible.Env (UserEnv)
import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Array as Array
import Data.Either (note)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Natural (Natural, intToNat, natToInt)
import Data.Slider (Slider(..))
import Data.Slider as Slider
import Data.String as String
import Data.Traversable (for_)
import Data.Tuple (snd)
import Data.ZipperArray as ZipperArray
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Tailwind as T
import Web.Event.Event (Event)
import Web.UIEvent.KeyboardEvent as KE

data Action
  = Initialize
  | Receive { currentUser :: Maybe Boolean }
  | Navigate Route Event
  | NewSlideContentChange String
  | AddSlide
  | ImgSelected String
  | ReceiveMsg ToCoordinator
  | SendMsg ToPresenter
  | PersistSlides
  | ClearSlides
    -- Controls
  | Start
  | TogglePause
  | Stop
  | Next
  | Prev
  | GoToSlide Natural
    -- Verse Picker
  | PickerSelectBook Bible.Book
  | PickerSelectChapter Bible.Book Bible.Chapter Int
  | PickerSelectVerse Bible.Book Bible.Chapter Int String Int -- ???
  | PickerClearChapter Bible.Book
  | PickerClear

data VersePicker
  = SelectedNone
  | SelectedBook Bible.Book
  | SelectedChapter Bible.Book Bible.Chapter Int

type State
  = { currentUser :: Maybe Boolean
    , slides :: Slider Slide
    , newSlide :: String
    , bgImage :: String
    , channel :: Maybe (Presentation.Channel ToPresenter)
    , bible :: RemoteData String Bible
    , versePicker :: VersePicker
    }

imgA :: String
imgA = "https://images.unsplash.com/photo-1506744038136-46273834b3fb?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1950&q=80"

imgs :: Array String
imgs =
  [ imgA
  , "https://images.unsplash.com/photo-1611030225755-3082ed97e11e?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1951&q=80"
  , "https://images.unsplash.com/photo-1611569689188-98d6173842c1?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1950&q=80"
  , "https://images.unsplash.com/photo-1588392382834-a891154bca4d?ixlib=rb-1.2.1&ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&auto=format&fit=crop&w=1355&q=80"
  , "https://images.unsplash.com/photo-1470071459604-3b5ec3a7fe05?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1440&q=80"
  , "https://images.unsplash.com/photo-1418065460487-3e41a6c84dc5?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1350&q=80"
  ]

component
  :: forall q o m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => ManageBible m
  => Navigate m
  => LocalStorage m
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
    , bible: NotAsked
    , versePicker: SelectedNone
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.fork do
        { source, channel } <- H.liftEffect $ Presentation.mkChannel "channel" ReceiveMsg
        void $ H.subscribe source
        H.modify_ _ { channel = Just channel }

      void $ H.fork do
        H.modify_ _ { bible = Loading }
        bible <- RemoteData.fromEither <$> note "Could not download Bible" <$> downloadBible "es_rvr_map.json"
        H.modify_ _ { bible = bible }
      mbSlides <- getItem "slides"
      for_ mbSlides $ \slides -> H.modify_ _ { slides = Inactive slides }

    ReceiveMsg GetState -> do
      {bgImage, slides} <- H.get
      handleAction $ SendMsg $ SetSlide
        $ case slides of
            Active ss -> ZipperArray.current ss
            _ -> { background: Just bgImage, content: Still }

    SendMsg msg -> do
      {channel} <- H.get
      case channel of
        Just { postMessage } ->  H.liftEffect $ postMessage msg
        Nothing -> pure unit

    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

    Navigate route e -> navigate_ e route

    AddSlide -> do
      {newSlide, bgImage} <- H.get
      case newSlide of
        "" -> pure unit
        contents -> do
          H.modify_ $ \s -> s { newSlide = "", slides = Slider.snoc s.slides $ { background: Just bgImage, content: Text contents } }
          handleAction PersistSlides

    NewSlideContentChange contents ->
      H.modify_ _ { newSlide = contents }

    ImgSelected img ->
      H.modify_ _ { bgImage = img }

    -- TODO: lenses
    TogglePause -> do
      {slides, bgImage} <- H.get
      case slides of
        Inactive _ -> pure unit
        Active as -> do
          H.modify_ _ { slides = Paused as }
          handleAction $ SendMsg $ SetSlide { background: Just bgImage, content: Still }
        Paused as -> do
          H.modify_ _ { slides = Active as }
          handleAction $ SendMsg $ SetSlide $ ZipperArray.current as

    -- TODO: lenses
    Stop -> do
      {slides} <- H.get
      case slides of
        Inactive _ -> pure unit
        Active as -> H.modify_ _ { slides = Inactive $ ZipperArray.toArray as }
        Paused as -> H.modify_ _ { slides = Inactive $ ZipperArray.toArray as }

    -- TODO: lenses
    Start -> do
      {slides} <- H.get
      case slides of
        Inactive as ->
          case ZipperArray.fromArray as of
            Nothing -> pure unit
            Just asZip -> do
               H.modify_ _ { slides = Active asZip }
               handleAction $ SendMsg $ SetSlide $ ZipperArray.current asZip
        _ -> pure unit

    -- TODO: lenses
    GoToSlide n -> do
      {slides} <- H.get
      case slides of
        -- TODO: switch to active
        Active as ->
          case ZipperArray.goIndex n as of
            Nothing -> pure unit
            Just updated -> do
              handleAction $ SendMsg $ SetSlide $ ZipperArray.current updated
              H.modify_ _ { slides = Active updated }
        _ -> pure unit

    -- TODO: lenses
    Prev -> do
      {slides} <- H.get
      case slides of
        Active as ->
          case ZipperArray.goPrev as of
            Nothing -> pure unit
            Just updated -> do
              handleAction $ SendMsg $ SetSlide $ ZipperArray.current updated
              H.modify_ _ { slides = Active updated }
        _ -> pure unit

    -- TODO: lenses
    Next -> do
      {slides} <- H.get
      case slides of
        Active as ->
          case ZipperArray.goNext as of
            Nothing -> pure unit
            Just updated -> do
              handleAction $ SendMsg $ SetSlide $ ZipperArray.current updated
              H.modify_ _ { slides = Active updated }
        _ -> pure unit

    ClearSlides -> do
      {bgImage} <- H.get
      H.modify_ _ { slides = Inactive [] }
      removeItem "slides"
      handleAction $ SendMsg $ SetSlide { background: Just bgImage, content: Still }

    PersistSlides -> do
      slides <- H.gets (Slider.toArray <<<_.slides)
      setItem "slides" slides

    PickerSelectBook book ->
      H.modify_ _ { versePicker = SelectedBook book }

    PickerSelectChapter book chapter n ->
      H.modify_ _ { versePicker = SelectedChapter book chapter n }

    PickerSelectVerse book chapter n verse i -> do
      {bgImage} <- H.get
      let content = Verse { book: book.name, chapter: n, verse: i, contents: verse }
          slide = { background: Just bgImage, content }
      H.modify_ $ \s -> s { versePicker = SelectedNone, slides = Slider.snoc s.slides slide }
      handleAction PersistSlides

    PickerClearChapter book ->
      H.modify_ _ { versePicker = SelectedBook book }

    PickerClear ->
      H.modify_ _ { versePicker = SelectedNone }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { currentUser, newSlide, slides, bgImage, bible, versePicker } =
    Layout.dashboard currentUser Navigate (Just Dashboard) content

    where

    newSlideEl "" = HH.text ""
    newSlideEl contents =
      -- TODO: better _new slide_ styles
      HH.div
        [ HP.classes
            [ T.bgGray200
            , T.textGray400
            , T.flex
            , T.justifyCenter
            , T.itemsCenter
            , T.flexCol
            , T.py12
            , T.px4
            , T.roundedMd
            , T.bgCenter
            , T.bgCover
            , T.border2
            , T.borderGray400
            ]
        ]
        $ Slide.multiline contents

    curSlide :: Int -> Slide -> _
    curSlide _ slide =
      HH.div
        [ HP.classes
            [ T.bgGray300
            , T.textWhite
            , T.flex
            , T.justifyCenter
            , T.itemsCenter
            , T.flexCol
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
            , T.bgCenter
            , T.bgCover
            ]
        , HP.prop (H.PropName "style") $ "background-image: url('" <> fromMaybe imgA slide.background <> "');"
        ]
        [ case slide.content of
            Still -> HH.text ""
            Text str -> HH.div [] $ Slide.multiline str
            Verse {book, chapter, verse} -> Slide.highlight $ book <> " " <> show chapter <> ":" <> show verse
        ]

    restSlide :: Natural -> Int -> Slide -> _
    restSlide offset i slide =
      HH.div
        [ HE.onDoubleClick $ \_ -> Just $ GoToSlide $ offset + intToNat i
        , HP.classes
            [ T.bgGray300
            , T.textWhite
            , T.flex
            , T.justifyCenter
            , T.itemsCenter
            , T.flexCol
            , T.py12
            , T.px4
            , T.roundedMd
            , T.hoverRing2
            , T.hoverRingOffset2
            , T.hoverRingGreen400
            , T.selectNone
            , T.bgCenter
            , T.bgCover
            ]
        , HP.prop (H.PropName "style") $ "background-image: url('" <> fromMaybe imgA slide.background <> "');"
        ]
        [ case slide.content of
            Still -> HH.text ""
            Text str -> HH.div [] $ Slide.multiline str
            Verse {book, chapter, verse} -> Slide.highlight $ book <> " " <> show chapter <> ":" <> show verse
        ]

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

    bookButton :: Bible.Book -> _
    bookButton book =
      HH.button
        [ HP.classes [ T.py2, T.px1, T.border, T.borderGray400, T.selectNone ]
        , HP.type_ HP.ButtonButton
        , HE.onClick \_ -> Just $ PickerSelectBook book
        ]
        [ HH.text book.name ]

    chapterButton :: Bible.Book -> Int -> Bible.Chapter -> _
    chapterButton book n chapter =
      HH.button
        [ HP.classes [ T.py2, T.px1, T.border, T.borderGray400, T.selectNone ]
        , HP.type_ HP.ButtonButton
        , HE.onClick \_ -> Just $ PickerSelectChapter book chapter n
        ]
        [ HH.text $ show n ]

    verseButton :: Bible.Book -> Int -> Bible.Chapter -> Int -> String -> _
    verseButton book n chapter i verse =
      HH.button
        [ HP.classes [ T.py2, T.px1, T.border, T.borderGray400, T.selectNone ]
        , HP.type_ HP.ButtonButton
        , HE.onClick \_ -> Just $ PickerSelectVerse book chapter n verse i
        ]
        [ HH.text $ show i ]

    chapterEl :: Bible.Book -> Bible.Chapter -> Int -> _
    chapterEl book chapter n =
      HH.div
        [ HP.classes [ T.mt4 ] ]
        [ HH.div
            [ HP.classes [ T.textGray700, T.mt4 ] ]
            [ HH.text $ book.name <> " " <> show n ]
        , HH.div
            [ HP.classes [ T.mt4, T.grid, T.gridCols10, T.gap2 ] ]
            $ map snd
            $ Map.toUnfoldable
            $ mapWithIndex (verseButton book n chapter) chapter
        ]

    bookEl :: Bible.Book -> _
    bookEl book =
      HH.div
        [ HP.classes [ T.mt4 ] ]
        [ HH.div
            [ HP.classes [ T.textGray700, T.mt4 ] ]
            [ HH.text book.name ]
        , HH.div
            [ HP.classes [ T.mt4, T.grid, T.gridCols10, T.gap2 ] ]
            $ map snd
            $ Map.toUnfoldable
            $ mapWithIndex (chapterButton book) book.chapters
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
                , button "Add Slide" (String.null newSlide) $ Just AddSlide
                , HH.div
                    [ HP.classes [ T.textGray700, T.mt8 ] ]
                    [ HH.text "Set background" ]
                , HH.div
                    [ HP.classes [ T.grid, T.gridCols4, T.gap4, T.mt2 ] ]
                    $ map imgEl imgs
                , HH.div
                    [ HP.classes [ T.textGray700, T.textLg, T.mt8 ] ]
                    [ HH.text "Chose a Bible verse" ]
                , case bible of
                    NotAsked -> HH.text ""
                    Loading ->
                      HH.div
                        [ HP.classes [ T.textGray700, T.mt4 ] ]
                        [ HH.text "..." ]
                    Success b ->
                      case versePicker of
                        SelectedNone ->
                          HH.div
                            [ HP.classes [ T.textGray700, T.mt4, T.grid, T.gridCols6, T.gap2 ] ]
                            $ map  bookButton b
                        SelectedBook book ->
                          HH.div
                            [ HP.classes [ T.mt4 ] ]
                            [ bookEl book ]
                        SelectedChapter book chapter n ->
                          HH.div
                            [ HP.classes [ T.mt4 ] ]
                            [ chapterEl book chapter n ]
                    Failure msg ->
                      HH.div
                        [ HP.classes [ T.textRed600, T.mt4 ] ]
                        [ HH.text msg ]
                ]
            ]
        , HH.div
            []
            [ HH.div
                [ HP.classes [ T.textGray700, T.textLg, T.mb2 ] ]
                [ HH.text "Slides queue" ]
            -- TODO better location
            -- TODO button alert color
            , HH.div
                [ HP.classes [ T.mb2 ] ]
                [ button "Clear slides" false $ Just ClearSlides ]
            , case slides of
                Active as ->
                  HH.div
                    [ HP.classes [ T.grid, T.gridCols4, T.gap4 ] ]
                    [ HH.div [ HP.classes [ T.colSpan1 ] ] [ button "Prev" (ZipperArray.atStart as) $ Just Prev ]
                    , HH.div [ HP.classes [ T.colSpan2 ] ] [ button "Pause" false $ Just TogglePause ]
                    , HH.div [ HP.classes [ T.colSpan1 ] ] [ button "Next" (ZipperArray.atEnd as) $ Just Next ]
                    , HH.div [ HP.classes [ T.colSpan4 ] ] [ button "Stop" false $ Just Stop ]
                    ]
                Paused as ->
                  HH.div
                    [ HP.classes [ T.grid, T.gridCols4, T.gap4 ] ]
                    [ HH.div [ HP.classes [ T.colSpan1 ] ] [ button "Prev" true Nothing ]
                    , HH.div [ HP.classes [ T.colSpan2 ] ] [ button "Resume" false $ Just TogglePause ]
                    , HH.div [ HP.classes [ T.colSpan1 ] ] [ button "Next" true Nothing ]
                    , HH.div [ HP.classes [ T.colSpan4 ] ] [ button "Stop" false $ Just Stop ]
                    ]
                Inactive as ->
                  button "Start" (Array.null as) $ Just Start
            , case slides of
                Active slides' ->
                  HH.div
                    [ HP.classes [ T.mt6, T.flex, T.flexCol, T.gap4 ] ]
                    $ Array.cons (curSlide (natToInt $ ZipperArray.curIndex slides') (ZipperArray.current slides'))
                    $ flip Array.snoc (newSlideEl newSlide)
                    $ mapWithIndex (restSlide $ ZipperArray.curIndex slides' + intToNat 1)
                    $ ZipperArray.succ slides'
                Paused slides' ->
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

-- TODO move to components
-- TODO support colors
button :: forall i p. String -> Boolean -> Maybe p -> HH.HTML i p
button text disabled action =
  HH.button
    [ HP.type_ HP.ButtonButton
    , HE.onClick \_ -> action
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
    , HP.disabled disabled
    ]
    [ HH.text text ]
