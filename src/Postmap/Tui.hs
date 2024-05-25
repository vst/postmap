{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Postmap.Tui where

import qualified Brick
import qualified Brick.Widgets.Border
import qualified Brick.Widgets.Core
import qualified Brick.Widgets.List
import Control.Monad (void)
import Data.Maybe (catMaybes, fromMaybe)
import Data.String.Interpolate (i)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import Postmap.Introspect (ColumnName (..), TableName (..), TableSchemaName (..))
import qualified Postmap.Meta as Meta
import Postmap.Spec (Field (..), FieldName (..), FieldReference (..), Record (..), RecordName (..), Spec (..))


-- * Application


runTui :: Spec -> IO ()
runTui spec =
  void $ Brick.defaultMain app initialState
  where
    app :: AppType
    app =
      Brick.App
        { Brick.appDraw = appDraw
        , Brick.appChooseCursor = Brick.showFirstCursor
        , Brick.appHandleEvent = appHandleEvent
        , Brick.appStartEvent = pure ()
        , Brick.appAttrMap = const appAttrs
        }
    initialState = initAppState spec


type AppType = Brick.App AppState AppEvent AppNames


-- * Names


data AppNames
  = AppNamesRecordList
  | AppNamesRecordDetailsName
  | AppNamesRecordDetailsTitle
  | AppNamesRecordDetailsTableSchema
  | AppNamesRecordDetailsTableName
  | AppNamesRecordDetailsDescription
  | AppNamesRecordDetailsUniques
  | AppNamesRecordDetailsFields
  deriving (Bounded, Enum, Eq, Ord, Show)


-- * State


data AppState = AppState
  { _appStateSpec :: !Spec
  , _appStateSpecPath :: !(Maybe FilePath)
  , _appStateFocus :: !AppNames
  , _appStateRecordList :: !AppStateRecordList
  , _appStateRecordDetailsUniques :: !AppStateRecordDetailUniques
  , _appStateRecordDetailsFields :: !AppStateRecordDetailFields
  }


initAppState :: Spec -> AppState
initAppState spec =
  let recordList = mkListRecordsState spec
      recordDetailsUniques =
        case Brick.Widgets.List.listSelectedElement recordList of
          Nothing -> mkListRecordRecordUniques []
          Just (_, Record {..}) -> mkListRecordRecordUniques recordUniques
      recordDetailsUniques1 =
        case Brick.Widgets.List.listSelectedElement recordList of
          Nothing -> mkListRecordRecordFields []
          Just (_, Record {..}) -> mkListRecordRecordFields recordFields
   in AppState
        { _appStateSpec = spec
        , _appStateSpecPath = Nothing
        , _appStateFocus = AppNamesRecordList
        , _appStateRecordList = recordList
        , _appStateRecordDetailsUniques = recordDetailsUniques
        , _appStateRecordDetailsFields = recordDetailsUniques1
        }


-- ** Record List


type AppStateRecordList = Brick.Widgets.List.List AppNames Record


mkListRecordsState :: Spec -> AppStateRecordList
mkListRecordsState =
  flip (Brick.Widgets.List.list AppNamesRecordList) 1 . V.fromList . specRecords


-- ** Uniques List


type AppStateRecordDetailUniques = Brick.Widgets.List.List AppNames [FieldName]


mkListRecordRecordUniques :: [[FieldName]] -> AppStateRecordDetailUniques
mkListRecordRecordUniques =
  flip (Brick.Widgets.List.list AppNamesRecordDetailsUniques) 1 . V.fromList


-- ** Fields List


type AppStateRecordDetailFields = Brick.Widgets.List.List AppNames Field


mkListRecordRecordFields :: [Field] -> AppStateRecordDetailFields
mkListRecordRecordFields =
  flip (Brick.Widgets.List.list AppNamesRecordDetailsFields) 1 . V.fromList


-- * Events


-- ** Event Type


data AppEvent


-- ** Event Handler


type BrickAppEvent = Brick.BrickEvent AppNames AppEvent


type BrickAppEventM = Brick.EventM AppNames AppState ()


appHandleEvent :: BrickAppEvent -> BrickAppEventM
appHandleEvent (Brick.VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl])) = Brick.halt
appHandleEvent (Brick.VtyEvent (Vty.EvKey (Vty.KChar '\t') [])) = appHandleToggleFocus False
appHandleEvent (Brick.VtyEvent (Vty.EvKey Vty.KBackTab [])) = appHandleToggleFocus True
appHandleEvent (Brick.VtyEvent ev) = Brick.gets _appStateFocus >>= appHandleEventVty ev
appHandleEvent _ = pure ()


appHandleToggleFocus :: Bool -> BrickAppEventM
appHandleToggleFocus back = do
  focus <- Brick.gets _appStateFocus
  Brick.modify $ \state -> state {_appStateFocus = (if back then cyclePrev else cycleNext) focus}


appHandleEventVty :: Vty.Event -> AppNames -> BrickAppEventM
appHandleEventVty ev AppNamesRecordList = appHandleEventVtyRecordList ev
appHandleEventVty ev AppNamesRecordDetailsDescription = appHandleEventVtyRecordDetailsDescription ev
appHandleEventVty ev AppNamesRecordDetailsUniques = appHandleEventVtyRecordDetailsUniques ev
appHandleEventVty ev AppNamesRecordDetailsFields = appHandleEventVtyRecordDetailsFields ev
appHandleEventVty _ _ = pure ()


appHandleEventVtyRecordList :: Vty.Event -> BrickAppEventM
appHandleEventVtyRecordList ev = do
  l <- Brick.gets _appStateRecordList
  (nl, _) <- Brick.nestEventM l $ Brick.Widgets.List.handleListEvent ev
  let nrdu = case Brick.Widgets.List.listSelectedElement nl of
        Nothing -> mkListRecordRecordUniques []
        Just (_, Record {..}) -> mkListRecordRecordUniques recordUniques
      nrdf = case Brick.Widgets.List.listSelectedElement nl of
        Nothing -> mkListRecordRecordFields []
        Just (_, Record {..}) -> mkListRecordRecordFields recordFields
  Brick.modify $ \state ->
    state
      { _appStateRecordList = nl
      , _appStateRecordDetailsUniques = nrdu
      , _appStateRecordDetailsFields = nrdf
      }


appHandleEventVtyRecordDetailsUniques :: Vty.Event -> BrickAppEventM
appHandleEventVtyRecordDetailsUniques ev = do
  l <- Brick.gets _appStateRecordDetailsUniques
  (nl, _) <- Brick.nestEventM l $ Brick.Widgets.List.handleListEvent ev
  Brick.modify $ \state -> state {_appStateRecordDetailsUniques = nl}


appHandleEventVtyRecordDetailsFields :: Vty.Event -> BrickAppEventM
appHandleEventVtyRecordDetailsFields ev = do
  l <- Brick.gets _appStateRecordDetailsFields
  (nl, _) <- Brick.nestEventM l $ Brick.Widgets.List.handleListEvent ev
  Brick.modify $ \state -> state {_appStateRecordDetailsFields = nl}


vpAppNamesRecordDetailsDescription :: Brick.ViewportScroll AppNames
vpAppNamesRecordDetailsDescription = Brick.viewportScroll AppNamesRecordDetailsDescription


appHandleEventVtyRecordDetailsDescription :: Vty.Event -> BrickAppEventM
appHandleEventVtyRecordDetailsDescription (Vty.EvKey Vty.KDown []) = Brick.vScrollBy vpAppNamesRecordDetailsDescription 1
appHandleEventVtyRecordDetailsDescription (Vty.EvKey Vty.KUp []) = Brick.vScrollBy vpAppNamesRecordDetailsDescription (-1)
appHandleEventVtyRecordDetailsDescription (Vty.EvKey Vty.KRight []) = Brick.hScrollBy vpAppNamesRecordDetailsDescription 1
appHandleEventVtyRecordDetailsDescription (Vty.EvKey Vty.KLeft []) = Brick.hScrollBy vpAppNamesRecordDetailsDescription (-1)
appHandleEventVtyRecordDetailsDescription _ = pure ()


-- * Views


type AppWidget = Brick.Widget AppNames


appDraw :: AppState -> [AppWidget]
appDraw st =
  [ appUi st
  ]


appUi :: AppState -> AppWidget
appUi st =
  Brick.vBox
    [ Brick.hBox
        [ Brick.hLimitPercent 30 $ renderRecordList st
        , Brick.hLimitPercent 50 $ renderRecordPreview st
        , Brick.hLimitPercent 100 $ renderRecordFieldPreview st
        ]
    , Brick.Widgets.Border.hBorderWithLabel title
    ]
  where
    title = Brick.str (" postmap " <> Meta.versionString <> " ")


-- ** Record List


renderRecordList :: AppState -> AppWidget
renderRecordList AppState {..} =
  highlightBorders (_appStateFocus == AppNamesRecordList)
    . Brick.Widgets.Border.borderWithLabel (Brick.str " Records List ")
    $ Brick.Widgets.List.renderList renderRecordListItem True _appStateRecordList


renderRecordListItem :: Bool -> Record -> AppWidget
renderRecordListItem isSelected record =
  if isSelected then paintSelected widget else widget
  where
    label = [Just (unRecordName (recordName record)), recordTitle record]
    widget = Brick.txt . T.intercalate " - " $ catMaybes label
    paintSelected = Brick.withAttr (Brick.Widgets.List.listSelectedAttr <> Brick.attrName "highlight-list-item")


-- ** Record Preview


renderRecordPreview :: AppState -> AppWidget
renderRecordPreview st@AppState {..} =
  case Brick.Widgets.List.listSelectedElement _appStateRecordList of
    Nothing -> Brick.str "-- No record selected --"
    Just (_, Record {..}) ->
      Brick.vBox
        [ boxed 1 AppNamesRecordDetailsName "Name" (unRecordName recordName)
        , boxed 1 AppNamesRecordDetailsTitle "Title" (fromMaybe "<untitled>" recordTitle)
        , Brick.hBox
            [ Brick.hLimitPercent 30 $ boxed 1 AppNamesRecordDetailsTableSchema "Table Schema" (unTableSchemaName recordTableSchema)
            , Brick.hLimitPercent 100 $ boxed 1 AppNamesRecordDetailsTableName "Table Name" (unTableName recordTableName)
            ]
        , boxed 6 AppNamesRecordDetailsDescription "Description" (fromMaybe "<no description>" recordDescription)
        , Brick.vBox
            [ Brick.vLimitPercent 30 $ renderRecordDetailsUniquesList st
            , Brick.vLimitPercent 100 $ renderRecordDetailsFieldList st
            ]
        ]
  where
    boxed c n l v = highlightBorders (_appStateFocus == n) . Brick.vLimit (c + 2) . Brick.Widgets.Border.borderWithLabel (Brick.txt l) . Brick.viewport n Brick.Both $ Brick.txt v


-- *** Unique Fields


renderRecordDetailsUniquesList :: AppState -> AppWidget
renderRecordDetailsUniquesList AppState {..} =
  highlightBorders (_appStateFocus == AppNamesRecordDetailsUniques)
    . Brick.Widgets.Border.borderWithLabel (Brick.str " Unique Fields ")
    $ Brick.Widgets.List.renderList renderRecordDetailsUniquesListItem True _appStateRecordDetailsUniques


renderRecordDetailsUniquesListItem :: Bool -> [FieldName] -> AppWidget
renderRecordDetailsUniquesListItem isSelected fns =
  if isSelected then paintSelected widget else widget
  where
    widget = Brick.txt . T.intercalate ", " $ fmap unFieldName fns
    paintSelected = Brick.withAttr (Brick.Widgets.List.listSelectedAttr <> Brick.attrName "highlight-list-item")


-- ***  Fields


renderRecordDetailsFieldList :: AppState -> AppWidget
renderRecordDetailsFieldList AppState {..} =
  highlightBorders (_appStateFocus == AppNamesRecordDetailsFields)
    . Brick.Widgets.Border.borderWithLabel (Brick.str " Fields ")
    $ Brick.Widgets.List.renderList renderRecordDetailsFieldListItem True _appStateRecordDetailsFields


renderRecordDetailsFieldListItem :: Bool -> Field -> AppWidget
renderRecordDetailsFieldListItem isSelected Field {..} =
  if isSelected then paintSelected widget else widget
  where
    widget =
      Brick.hBox
        [ Brick.txt [i|#{unFieldName fieldName} (#{fromMaybe "<unknown>" fieldType})|]
        , Brick.txt " - "
        , Brick.txt [i|#{unColumnName fieldColumnName} (#{fieldColumnType})|]
        ]
    paintSelected = Brick.withAttr (Brick.Widgets.List.listSelectedAttr <> Brick.attrName "highlight-list-item")


-- ** Record Field Preview


renderRecordFieldPreview :: AppState -> AppWidget
renderRecordFieldPreview AppState {..} =
  case Brick.Widgets.List.listSelectedElement _appStateRecordDetailsFields of
    Nothing -> Brick.str "-- No recordfield selected --"
    Just (_, Field {..}) ->
      Brick.vBox
        [ boxed "Name" (unFieldName fieldName)
        , boxed "Type" (fromMaybe "<unknown>" fieldType)
        , boxed "Column Name" (unColumnName fieldColumnName)
        , boxed "Column Type" fieldColumnType
        , boxed "Not Nullable" $ if fieldNotNullable then "Not-Nullable" else "Nullable"
        , boxed "Is Primary Key" $ if fieldIsPrimaryKey then "Primary Key" else "-"
        , boxed "Is Unique" $ if fieldIsUnique then "Unique" else "-"
        , boxed "Reference" (maybe "<no reference>" (\x -> unRecordName (fieldReferenceRecord x) <> "." <> unFieldName (fieldReferenceField x)) fieldReference)
        , Brick.Widgets.Border.borderWithLabel (Brick.txt "Description") $
            Brick.hBox
              [ Brick.txt $ fromMaybe "<no description>" fieldDescription
              , Brick.fill ' '
              ]
        ]
  where
    boxed l v = Brick.Widgets.Border.borderWithLabel (Brick.txt l) (Brick.txt v Brick.<=> Brick.fill ' ')


-- ** Attributes


appAttrs :: Brick.AttrMap
appAttrs =
  Brick.attrMap
    Vty.defAttr
    [ (Brick.Widgets.List.listAttr, Vty.defAttr)
    , (Brick.Widgets.List.listSelectedAttr, Vty.black `Brick.on` Vty.cyan)
    ]


appAttrsHighlightBorders :: [(Brick.AttrName, Vty.Attr)]
appAttrsHighlightBorders =
  [ (Brick.Widgets.Border.borderAttr, Vty.withStyle (Brick.fg Vty.yellow) Vty.bold)
  ]


highlightBorders :: Bool -> Brick.Widget n -> Brick.Widget n
highlightBorders False = id
highlightBorders True = Brick.Widgets.Core.updateAttrMap (Brick.applyAttrMappings appAttrsHighlightBorders)


-- * Helpers


-- | Cycle through the 'Bounded' and 'Enum' instance.
cyclePrev :: (Eq a, Enum a, Bounded a) => a -> a
cyclePrev x = if x == minBound then maxBound else pred x


-- | Cycle through the 'Bounded' and 'Enum' instance.
cycleNext :: (Eq a, Enum a, Bounded a) => a -> a
cycleNext x = if x == maxBound then minBound else succ x
