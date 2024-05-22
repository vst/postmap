{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Postmap.Tui.App where

import qualified Brick
import qualified Brick.AttrMap
import qualified Brick.Widgets.Border
import qualified Brick.Widgets.List
import qualified Brick.Widgets.List as Brick.Widgets
import Control.Monad (void)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import Postmap.Introspect (TableName (unTableName), TableSchemaName (unTableSchemaName))
import qualified Postmap.Meta as Meta
import Postmap.Spec.Types (Field (..), FieldName (..), Record (..), RecordName (unRecordName), Spec (..))
import Postmap.Tui.AppEvent (AppEvent, appHandleEvent)
import Postmap.Tui.AppState (AppPerspective (..), AppState (..), initAppState)


tui :: Spec -> IO ()
tui spec =
  void $ Brick.defaultMain app initialState
  where
    app :: AppType
    app =
      Brick.App
        { Brick.appDraw = appDraw
        , Brick.appChooseCursor = Brick.showFirstCursor
        , Brick.appHandleEvent = appHandleEvent
        , Brick.appStartEvent = pure ()
        , Brick.appAttrMap = const appAttrMap
        }
    initialState = initAppState spec


type AppType = Brick.App AppState AppEvent ()


appDraw :: AppState -> [Brick.Widget ()]
appDraw _st =
  [ Brick.Widgets.Border.borderWithLabel title $ renderPerspective _st
  ]
  where
    title = Brick.str (" postmap " <> Meta.versionString <> " ")


renderPerspective :: AppState -> Brick.Widget ()
renderPerspective _st =
  case _appStatePerspective _st of
    AppPerspectiveAbout -> Brick.str "TODO: render about"
    AppPerspectiveRecords listRecordsState ->
      Brick.padLeftRight 3 . Brick.padTopBottom 1 $
        Brick.hBox
          [ Brick.hLimitPercent 30
              . Brick.Widgets.Border.borderWithLabel (Brick.str " Records List ")
              $ Brick.Widgets.renderList renderRecordListItem True listRecordsState
          , Brick.hLimitPercent 100
              . Brick.Widgets.Border.borderWithLabel (Brick.str " Record Details ")
              $ renderRecordDetails listRecordsState
          ]


recordList :: Spec -> Brick.Widgets.List () Record
recordList Spec {..} =
  Brick.Widgets.list () (V.fromList specRecords) 0


renderRecordListItem :: Bool -> Record -> Brick.Widget ()
renderRecordListItem isSelected record =
  if isSelected
    then paintSelected widget
    else widget
  where
    label = [Just (unRecordName (recordName record)), recordTitle record]
    widget = Brick.str . T.unpack . T.intercalate " - " $ catMaybes label
    paintSelected = Brick.withAttr (Brick.Widgets.List.listSelectedAttr <> Brick.AttrMap.attrName "highlight-list-item")


renderRecordDetails :: Brick.Widgets.List.List () Record -> Brick.Widget ()
renderRecordDetails listRecordsState =
  case Brick.Widgets.List.listSelectedElement listRecordsState of
    Nothing -> Brick.str "No record selected"
    Just (_, record) ->
      Brick.hBox
        [ Brick.vBox
            [ Brick.str "Name: " Brick.<+> Brick.str (T.unpack . unRecordName . recordName $ record)
            , Brick.str "Title: " Brick.<+> Brick.str (maybe "<empty>" T.unpack $ recordTitle record)
            , Brick.str "Description: " Brick.<+> Brick.str (maybe "<empty>" T.unpack $ recordDescription record)
            , Brick.str "Table Name: " Brick.<+> Brick.str (T.unpack . unTableName $ recordTableName record)
            , Brick.str "Table Schema: " Brick.<+> Brick.str (T.unpack . unTableSchemaName $ recordTableSchema record)
            , Brick.str "Uniques: " Brick.<+> Brick.vBox (fmap renderUnique (recordUniques record))
            , Brick.Widgets.Border.borderWithLabel (Brick.str " Fields ") $
                Brick.vBox (fmap renderField (recordFields record))
            ]
        , Brick.fill ' '
        ]


renderField :: Field -> Brick.Widget ()
renderField Field {..} =
  Brick.str (T.unpack (unFieldName fieldName <> " (" <> fieldType <> ")"))


renderUnique :: [FieldName] -> Brick.Widget ()
renderUnique = Brick.str . T.unpack . T.intercalate ", " . fmap unFieldName


appAttrMap :: Brick.AttrMap.AttrMap
appAttrMap =
  Brick.AttrMap.attrMap
    Vty.defAttr
    [ (Brick.Widgets.List.listAttr, Vty.defAttr)
    , (Brick.Widgets.List.listSelectedAttr, Vty.black `Brick.on` Vty.cyan)
    ]
