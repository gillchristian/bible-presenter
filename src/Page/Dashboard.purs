module Bible.Page.Dashboard where

import Prelude

import Bible.Capability.LocalStorage (class LocalStorage, getItem, removeItem, setItem)
import Bible.Capability.Navigate (class Navigate, navigate_)
import Bible.Capability.Resource.Bible (class ManageBible, downloadBible)
import Bible.Component.HTML.Breadcrumbs as Breadcrumbs
import Bible.Component.HTML.Icons as Icons
import Bible.Component.HTML.Layout as Layout
import Bible.Component.HTML.Slide as Slide
import Bible.Component.HTML.Tabs (tabs)
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
import Data.Array.NonEmpty (cons')
import Data.Either (note)
import Data.Filterable (filter)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Natural (Natural, intToNat)
import Data.Slider (Slider(..))
import Data.Slider as Slider
import Data.String as String
import Data.Traversable (for_)
import Data.Tuple (Tuple(..), snd)
import Data.ZipperArray as ZipperArray
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Tailwind as T
import Web.Event.Event (Event)
import Web.UIEvent.KeyboardEvent as KE

data Tab
  = TabBible
  | TabText
  | TabBg

derive instance eqTab :: Eq Tab

data Action
  = Initialize
  | Receive {currentUser :: Maybe Boolean}
  | Navigate Route Event
  | NewSlideContentChange String
  | AddSlide
  | ImgSelected String
  | ReceiveMsg ToCoordinator
  | SendMsg ToPresenter
  | PersistSlides
  | ClearSlides
    -- Controls
  | Start (Maybe Natural)
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
    -- Tabs
  | ToggleTab Tab

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
    , activeTab :: Tab
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
  , "https://images.unsplash.com/photo-1517299321609-52687d1bc55a?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1950&q=80"
  , "https://images.unsplash.com/photo-1542601098-8fc114e148e2?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1950&q=80"
  , "https://images.unsplash.com/photo-1476108621677-3c620901b5e7?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1934&q=80"
  , "https://images.unsplash.com/photo-1491002052546-bf38f186af56?ixlib=rb-1.2.1&ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&auto=format&fit=crop&w=1983&q=80"
  , "https://images.unsplash.com/photo-1483921020237-2ff51e8e4b22?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1950&q=80"
  , "https://images.unsplash.com/photo-1464618663641-bbdd760ae84a?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1950&q=80"
  , "https://images.unsplash.com/photo-1547234935-80c7145ec969?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1953&q=80"
  , "https://images.unsplash.com/photo-1516653980844-c68df1de5249?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1950&q=80"
  , "https://images.unsplash.com/photo-1488866022504-f2584929ca5f?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1943&q=80"
  , "https://images.unsplash.com/photo-1470240731273-7821a6eeb6bd?ixlib=rb-1.2.1&ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&auto=format&fit=crop&w=1950&q=80"
  , "https://images.unsplash.com/photo-1544927093-98a95a4da1dd?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1950&q=80"
  , "https://images.unsplash.com/photo-1466590559380-c29c7f16d444?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1649&q=80"
  , "https://images.unsplash.com/photo-1562758619-78207a23a6e0?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1650&q=80"
  , "https://images.unsplash.com/uploads/14121010130570e22bcdf/e1730efe?ixlib=rb-1.2.1&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=1950&q=80"
  , "https://images.unsplash.com/photo-1541417904950-b855846fe074?ixlib=rb-1.2.1&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=1925&q=80"
  , "https://images.unsplash.com/photo-1552065413-8485dd9ae56c?ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&ixlib=rb-1.2.1&auto=format&fit=crop&w=2088&q=80"
  ]

bibleBreadcrumbs :: forall i p. Breadcrumbs.Crumb p -> Array (Breadcrumbs.Crumb p) -> HH.HTML i p
bibleBreadcrumbs = Breadcrumbs.breadcrumbs Icons.openBook

component
  :: forall q o m r
   . MonadAff m
  => MonadAsk {userEnv :: UserEnv | r} m
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
  initialState {currentUser} =
    { currentUser
    , slides: Inactive []
    , newSlide: ""
    , bgImage: imgA
    , channel: Nothing
    , bible: NotAsked
    , versePicker: SelectedNone
    , activeTab: TabBible
    }

  stillSlide :: forall slots. H.HalogenM State Action slots o m Action
  stillSlide =
    SendMsg <<< SetSlide <<< (\bg -> {content: Still, background: Just bg}) <$> H.gets _.bgImage

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.fork do
        {source, channel} <- H.liftEffect $ Presentation.mkChannel "channel" ReceiveMsg
        void $ H.subscribe source
        H.modify_ _ {channel = Just channel}

      void $ H.fork do
        H.modify_ _ {bible = Loading}
        bible <- RemoteData.fromEither <$> note "Could not download Bible" <$> downloadBible "es_rvr_map.json"
        H.modify_ _ {bible = bible}

      mbSlides <- getItem "slides"
      for_ mbSlides $ \slides -> do
        {bgImage} <- H.get
        let newBgImage = fromMaybe bgImage $ _.background =<< Array.head slides
        H.modify_ _ {slides = Inactive slides, bgImage = newBgImage}

      handleAction =<< stillSlide

    ReceiveMsg GetState -> do
      {slides} <- H.get
      case slides of
        Active ss -> handleAction $ SendMsg $ SetSlide $ ZipperArray.current ss
        _ -> handleAction =<< stillSlide

    SendMsg msg -> do
      {channel} <- H.get
      case channel of
        Just {postMessage} ->  H.liftEffect $ postMessage msg
        Nothing -> pure unit

    Receive {currentUser} ->
      H.modify_ _ {currentUser = currentUser}

    Navigate route e -> navigate_ e route

    AddSlide -> do
      {newSlide, bgImage} <- H.get
      case newSlide of
        "" -> pure unit
        contents -> do
          H.modify_ $ \s -> s {newSlide = "", slides = Slider.snoc s.slides $ {background: Just bgImage, content: Text contents}}
          handleAction PersistSlides

    NewSlideContentChange contents ->
      H.modify_ _ {newSlide = contents}

    ImgSelected img -> do
      {slides} <- H.get
      H.modify_ _ {bgImage = img}
      case slides of
        Active _ -> pure unit
        Inactive _ -> handleAction =<< stillSlide
        Paused _ -> handleAction =<< stillSlide

    -- TODO: lenses
    TogglePause -> do
      {slides} <- H.get
      case slides of
        Inactive _ -> pure unit
        Active as -> do
          H.modify_ _ {slides = Paused as}
          handleAction =<< stillSlide
        Paused as -> do
          H.modify_ _ {slides = Active as}
          handleAction $ SendMsg $ SetSlide $ ZipperArray.current as

    -- TODO: lenses
    Stop -> do
      {slides} <- H.get
      case slides of
        Inactive _ -> pure unit
        Active as -> H.modify_ _ {slides = Inactive $ ZipperArray.toArray as}
        Paused as -> H.modify_ _ {slides = Inactive $ ZipperArray.toArray as}
      handleAction =<< stillSlide

    -- TODO: lenses
    Start n -> do
      {slides} <- H.get
      case slides of
        Inactive as ->
          case ZipperArray.goIndex (fromMaybe (intToNat 0) n) =<< ZipperArray.fromArray as of
            Nothing -> pure unit
            Just asZip -> do
               H.modify_ _ {slides = Active asZip}
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
              H.modify_ _ {slides = Active updated}
        _ -> do
          handleAction $ Start $ Just n

    -- TODO: lenses
    Prev -> do
      {slides} <- H.get
      case slides of
        Active as ->
          case ZipperArray.goPrev as of
            Nothing -> pure unit
            Just updated -> do
              handleAction $ SendMsg $ SetSlide $ ZipperArray.current updated
              H.modify_ _ {slides = Active updated}
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
              H.modify_ _ {slides = Active updated}
        _ -> pure unit

    ClearSlides -> do
      {bgImage} <- H.get
      H.modify_ _ {slides = Inactive []}
      removeItem "slides"
      handleAction =<< stillSlide

    PersistSlides -> do
      slides <- H.gets (Slider.toArray <<<_.slides)
      setItem "slides" slides

    PickerSelectBook book ->
      H.modify_ _ {versePicker = SelectedBook book}

    PickerSelectChapter book chapter n ->
      H.modify_ _ {versePicker = SelectedChapter book chapter n}

    PickerSelectVerse book chapter n verse i -> do
      {bgImage} <- H.get
      let content = Verse {book: book.name, chapter: n, verse: i, contents: verse}
          slide = {background: Just bgImage, content}
      H.modify_ $ \s -> s {slides = Slider.snoc s.slides slide}
      handleAction PersistSlides

    PickerClearChapter book ->
      H.modify_ _ {versePicker = SelectedBook book}

    PickerClear ->
      H.modify_ _ {versePicker = SelectedNone}

    ToggleTab tab ->
      H.modify_ _ {activeTab = tab}

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render {activeTab, currentUser, newSlide, slides, bgImage, bible, versePicker} =
    Layout.dashboard
      (Just "Dashboard")
      Navigate
      (Just Dashboard)
      content

    where
    activeSlide :: Boolean -> Slide -> _
    activeSlide paused slide =
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

    slideListElem :: Boolean -> Int -> Slide -> _
    slideListElem current i slide =
      HH.div
        [ HP.classes
            [ T.flex
            , T.itemsCenter
            , T.p1
            , T.mb2
            , T.roundedMd
            , T.cursorPointer
            , cx T.bgGray100 current
            ]
        , HE.onDoubleClick $ \_ -> filter (const $ not current) $ Just $ GoToSlide $ intToNat i
        ]
        [ HH.div
            [ HP.classes
                [ T.bgGray300
                , T.roundedMd
                , T.bgCenter
                , T.bgCover
                , T.h8
                , T.w12
                ]
            , HP.prop (H.PropName "style") $ "background-image: url('" <> fromMaybe imgA slide.background <> "');"
            ]
            []
        , HH.div
            [ HP.classes
                [ T.ml4
                , T.selectNone
                ]
            ]
            [ HH.text $ case slide.content of
                Still -> ""
                Text str -> str
                Verse {book, chapter, verse} -> book <> " " <> show chapter <> ":" <> show verse
            ]
        ]

    imgButton img =
      HH.button
        [ HE.onClick \_ -> Just $ ImgSelected img
        , HP.classes
            [ T.roundedMd
            , T.hoverRing2
            , T.hoverRingGreen400
            , T.hoverRingOffset2
            , T.focusRing2
            , T.focusRingGreen400
            , T.focusRingOffset2
            , T.focusOutlineNone
            , cx T.ring2 $ bgImage == img
            , cx T.ringOffset2 $ bgImage == img
            , cx T.ringIndigo500 $ bgImage == img
            , T.h48
            ]
        ]
        [ HH.img [ HP.classes [ T.hFull, T.wFull, T.objectCover, T.roundedMd, T.cursorPointer ], HP.src img ] ]

    bookButton :: Bible.Book -> _
    bookButton book =
      HH.button
        [ HP.classes [ T.py2, T.px1, T.border, T.borderGray400, T.selectNone ]
        , HP.type_ HP.ButtonButton
        , HE.onClick \_ -> Just $ PickerSelectBook book
        ]
        [ HH.text book.name ]

    chapterButton :: Bible.Book -> Int -> Bible.Chapter -> Tuple String _
    chapterButton book n chapter =
      Tuple
        (show n)
        $ HH.button
            [ HP.classes [ T.py2, T.px1, T.border, T.borderGray400, T.selectNone ]
            , HP.type_ HP.ButtonButton
            , HE.onClick \_ -> Just $ PickerSelectChapter book chapter n
            ]
            [ HH.text $ show n ]

    verseButton :: Bible.Book -> Int -> Bible.Chapter -> Int -> String -> Tuple String _
    verseButton book n chapter i verse =
      Tuple
        (show n <> "-" <> show i)
        $ HH.button
            [ HP.classes [ T.py2, T.px1, T.border, T.borderGray400, T.selectNone ]
            , HP.type_ HP.ButtonButton
            , HE.onClick \_ -> Just $ PickerSelectVerse book chapter n verse i
            ]
            [ HH.text $ show i ]

    chapterSelectorElem :: Bible.Book -> Bible.Chapter -> Int -> _
    chapterSelectorElem book chapter n =
      HH.div
        [ HP.classes [ T.mt4 ] ]
        [ bibleBreadcrumbs
            {action: Just PickerClear, label: "Bible"}
            [ {action: Just $ PickerClearChapter book, label:  book.name}
            , {action: Nothing, label:  show n}
            ]
        , HK.div
            [ HP.classes [ T.mt4, T.grid, T.gridCols10, T.gap2 ] ]
            $ map snd
            $ Map.toUnfoldable
            $ mapWithIndex (verseButton book n chapter) chapter
        ]

    bookSelectorElem :: Bible.Book -> _
    bookSelectorElem book =
      HH.div
        [ HP.classes [ T.mt4 ] ]
        [ bibleBreadcrumbs
            {action: Just PickerClear, label: "Bible"}
            [ {action: Nothing, label:  book.name} ]
        , HK.div
            [ HP.classes [ T.mt4, T.grid, T.gridCols10, T.gap2 ] ]
            $ map snd
            $ Map.toUnfoldable
            $ mapWithIndex (chapterButton book) book.chapters
        ]

    slidesHeadEl =
      HH.div
        [ HP.classes
            [ T.flex
            , T.itemsCenter
            , T.justifyBetween
            , T.pt4
            , T.pb2
            , T.mb2
            , T.borderB
            , T.borderGray200
            ]
        ]
        [ HH.h3
            [ HP.classes [ T.textLg, T.leading6, T.fontMedium, T.textGray600 ] ]
            [ HH.text "Slides" ]
        , HH.div
            [ HP.classes [ T.ml4, T.flexShrink0 ] ]
            [ HH.button
                [ HP.classes
                    [ T.inlineFlex
                    , T.itemsCenter
                    , T.justifyCenter
                    , T.px4
                    , T.py1
                    , T.border
                    , T.borderTransparent
                    , T.fontMedium
                    , T.roundedMd
                    , T.textRed700
                    , T.bgRed100
                    , T.hoverBgRed200
                    , T.focusOutlineNone
                    , T.focusRing2
                    , T.focusRingOffset2
                    , T.focusRingRed500
                    , T.smTextSm
                    , T.disabledCursorNotAllowed
                    , T.disabledTextGray500
                    ]
                , HP.type_ HP.ButtonButton
                , HE.onClick \_ -> filter (const $ not $ Slider.null slides) $ Just ClearSlides
                , HP.disabled $ Slider.null slides
                ]
                [ HH.text "Clear slides" ]
            ]
        ]

    content =
      HH.div
        [ HP.classes [ T.grid, T.gridCols3, T.gap4 ] ]
        [ HH.div
            [ HP.classes [ T.colSpan2 ] ]
            [ HH.div
                []
                [ tabs
                    $ cons'
                        {active: TabBible == activeTab, action: Just $ ToggleTab TabBible, label: "Bible verse slide"}
                    $ [ {active: TabText == activeTab, action: Just $ ToggleTab TabText, label: "Text slide"}
                      , {active: TabBg == activeTab, action: Just $ ToggleTab TabBg, label: "Background"}
                      ]
                , case activeTab of
                    TabBible ->
                      HH.div
                        [ HP.classes [ T.mt8 ] ]
                        [ case bible of
                            NotAsked -> HH.text ""
                            Loading ->
                              HH.div
                                [ HP.classes [ T.textGray700, T.mt4 ] ]
                                [ HH.text "..." ]
                            Success b ->
                              case versePicker of
                                SelectedNone ->
                                  HH.div
                                    [ HP.classes [ T.mt4 ] ]
                                    [ bibleBreadcrumbs {action: Nothing, label: "Bible"} []
                                    , HH.div
                                        [ HP.classes [ T.textGray700, T.mt4, T.grid, T.gridCols6, T.gap2 ] ]
                                        $ map  bookButton b
                                    ]
                                SelectedBook book ->
                                  HH.div
                                    [ HP.classes [ T.mt4 ] ]
                                    [ bookSelectorElem book ]
                                SelectedChapter book chapter n ->
                                  HH.div
                                    [ HP.classes [ T.mt4 ] ]
                                    [ chapterSelectorElem book chapter n ]
                            Failure msg ->
                              -- TODO: error msg
                              HH.div
                                [ HP.classes [ T.textRed600, T.mt4 ] ]
                                [ HH.text msg ]
                        ]
                    TabText ->
                      HH.div
                        [ HP.classes [ T.mt8 ] ]
                        [ HH.textarea
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
                        , button (HH.text "Add Slide") (String.null newSlide) $ Just AddSlide
                        ]
                    TabBg ->
                      HH.div
                        [ HP.classes [ T.mt8 ] ]
                        [ HH.div
                            [ HP.classes [ T.grid, T.gridCols4, T.gap4, T.mt2 ] ]
                            $ map imgButton imgs
                        ]
                ]
            ]
        , HH.div
            []
            [ slidesHeadEl
            ,case slides of
                Active as ->
                  HH.div
                    [ HP.classes [ T.flex, T.justifyBetween, T.itemsCenter, T.mb2 ] ]
                    [ playerButton Icons.stop false $ Just Stop
                    , playerButton Icons.prev (ZipperArray.atStart as) $ Just Prev
                    , playerButton Icons.pause false $ Just TogglePause
                    , playerButton Icons.next (ZipperArray.atEnd as) $ Just Next
                    ]
                Paused as ->
                  HH.div
                    [ HP.classes [ T.flex, T.justifyBetween, T.itemsCenter, T.mb2 ] ]
                    [ playerButton Icons.stop false $ Just Stop
                    , playerButton Icons.prev true Nothing
                    , playerButton Icons.play false $ Just TogglePause
                    , playerButton Icons.next true Nothing
                    ]
                Inactive as ->
                  HH.div
                    [ HP.classes [ T.flex, T.mb2 ] ]
                    [ playerButton Icons.play (Array.null as) $ Just $ Start Nothing ]
            , HH.div
                []
                $ Slider.toArray
                $ Slider.mapCurrentWithIndex {cur: slideListElem true, rest: slideListElem false} slides

            , case slides of
                Active as ->
                  HH.div
                    [ HP.classes [ T.mt6 ] ]
                    [ activeSlide false $ ZipperArray.current as ]
                Paused as ->
                  HH.div
                    [ HP.classes [ T.mt6 ] ]
                    [ activeSlide true $ ZipperArray.current as ]
                Inactive _ ->
                  HH.text ""
            ]
        ]

playerButton :: forall i p. Icons.Icon -> Boolean -> Maybe p -> HH.HTML i p
playerButton icon disabled action =
  HH.button
    [ HP.type_ HP.ButtonButton
    , HE.onClick \_ -> action
    , HP.classes
        [ T.disabledCursorNotAllowed
        , T.disabledTextGray300
        , T.textGray600
        , T.focusOutlineNone
        , T.cursorPointer
        ]
    , HP.disabled disabled
    ]
    [ icon [ Icons.classes [ T.h10, T.w10 ] ]
    ]

-- TODO move to components
-- TODO support colors
button :: forall i p. HH.HTML i p -> Boolean -> Maybe p -> HH.HTML i p
button label disabled action =
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
    [ label ]
