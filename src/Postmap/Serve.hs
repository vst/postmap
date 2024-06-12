{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Postmap.Serve where

import Data.Foldable (forM_)
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Effectful (IOE, (:>))
import Effectful.Concurrent (Concurrent, runConcurrent)
import GHC.Generics (Generic)
import Postmap.Introspect (ColumnName (..), TableName (..), TableSchemaName (unTableSchemaName))
import Postmap.Spec
import Text.Read (readMaybe)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))
import Web.Hyperbole (Eff)
import qualified Web.Hyperbole as WH
import Web.Hyperbole.Route (Route (..))
import qualified Zamazingo.Text as Z.Text


runWeb :: Spec -> IO ()
runWeb spec = do
  putStrLn "Starting schema editor on http://localhost:3003"
  WH.run 3003 (app spec)


app :: Spec -> WH.Application
app spec =
  WH.liveApp
    (WH.basicDocument "Postmap Schema Editor")
    (runApp . WH.routeRequest $ router spec)


runApp :: IOE :> es => Eff (Concurrent : es) a -> Eff es a
runApp =
  runConcurrent


data AppRoute
  = AppRouteHome
  | AppRouteRecord !RecordName
  | AppRouteAbout
  deriving (Eq, Generic)


instance Route AppRoute where
  defRoute = AppRouteHome


  routePath AppRouteHome = []
  routePath (AppRouteRecord name) = ["records", unRecordName name]
  routePath AppRouteAbout = ["about"]


  matchRoute [] = Just AppRouteHome
  matchRoute ["records", name] = either (const Nothing) (Just . AppRouteRecord) (mkRecordName name)
  matchRoute ["about"] = Just AppRouteAbout
  matchRoute _ = Nothing


router
  :: WH.Hyperbole :> es
  => Concurrent :> es
  => IOE :> es
  => Spec
  -> AppRoute
  -> Eff es WH.Response
router spec AppRouteHome = pageHome spec
router spec@Spec {..} (AppRouteRecord x) = pageRecord spec x $ find (\Record {..} -> recordName == x) specRecords
router _spec AppRouteAbout = pageAbout


pageHome
  :: WH.Hyperbole :> es
  => Spec
  -> Eff es WH.Response
pageHome spec = WH.view $ do
  canvas (mkSideBar Nothing spec) "hello"


mkSideBar :: Maybe RecordName -> Spec -> WH.View c ()
mkSideBar mrn Spec {..} = do
  WH.el (WH.bold . WH.fontSize 20) "Records"
  forM_ specRecords $ \Record {..} -> do
    WH.link
      (WH.routeUrl (AppRouteRecord recordName))
      (WH.fontSize 16 . (if Just recordName == mrn then WH.bold else id) . WH.color Primary)
      (WH.text $ unRecordName recordName)


pageRecord :: WH.Hyperbole :> es => Spec -> RecordName -> Maybe Record -> Eff es WH.Response
pageRecord spec name mRecord = WH.view $ do
  canvas (mkSideBar (Just name) spec) . WH.el (WH.pad 10) $ do
    WH.el (WH.bold . WH.fontSize 24) (WH.text $ "Record: " <> unRecordName name)
    case mRecord of
      Nothing -> WH.el (WH.fontSize 16) "Record not found."
      Just Record {..} -> WH.col (WH.gap 10) $ do
        WH.el (WH.fontSize 16) $ labelled "Title" (WH.text $ fromMaybe "<untitled>" recordTitle)
        WH.el (WH.fontSize 16) $ labelled "Description" (WH.text $ fromMaybe "<no description>" recordDescription)
        WH.el (WH.fontSize 16) $ labelled "Table Schema" (WH.text $ unTableSchemaName recordTableSchema)
        WH.el (WH.fontSize 16) $ labelled "Table Name" (WH.text $ unTableName recordTableName)
        WH.el (WH.fontSize 16) $ labelled "Is View?" (WH.text $ if recordIsView then "Yes" else "No")
        WH.el (WH.bold . WH.fontSize 18) "Uniques"
        uniquesTable recordUniques
        WH.el (WH.bold . WH.fontSize 18) "Fields"
        fieldsTable recordFields
  where
    labelled x s = WH.row id $ do
      WH.col id $ WH.el (WH.bold . WH.pad 6) (WH.text x)
      WH.col WH.grow $ WH.el (WH.pad 6) s
    uniquesTable uniques =
      WH.table id uniques $ do
        WH.tcol (WH.th hd "Unique / Unique Together") $ \unique -> WH.td cell . WH.text $ T.intercalate ", " (unFieldName <$> unique)
      where
        hd = cell . WH.bold
        cell = WH.pad 4 . WH.border 1
    fieldsTable fields =
      WH.table id fields $ do
        WH.tcol (WH.th hd "Name") $ \Field {..} -> WH.td cell . WH.text $ unFieldName fieldName
        WH.tcol (WH.th hd "Type") $ \Field {..} -> WH.td cell . WH.text $ fromMaybe "<no-type-given>" fieldType
        WH.tcol (WH.th hd "Column") $ \Field {..} -> WH.td cell . WH.text $ unColumnName fieldColumnName
        WH.tcol (WH.th hd "Column Type") $ \Field {..} -> WH.td cell . WH.text $ fieldColumnType
        WH.tcol (WH.th hd "Nullable") $ \Field {..} -> WH.td cell . WH.text $ if fieldNotNullable then "NOT NULL" else "NULL"
        WH.tcol (WH.th hd "Primary Key") $ \Field {..} -> WH.td cell . WH.text $ if fieldIsPrimaryKey then "PRIMARY KEY" else ""
        WH.tcol (WH.th hd "Unique") $ \Field {..} -> WH.td cell . WH.text $ if fieldIsUnique then "UNIQUE" else ""
        WH.tcol (WH.th hd "Reference") $ \Field {..} -> WH.td cell . WH.text $ maybe "" (\FieldReference {..} -> unRecordName fieldReferenceRecord <> "." <> unFieldName fieldReferenceField) fieldReference
        WH.tcol (WH.th hd "Description") $ \Field {..} -> WH.td cell . WH.text $ fromMaybe "<no-description>" fieldDescription
      where
        hd = cell . WH.bold
        cell = WH.pad 4 . WH.border 1


pageAbout
  :: WH.Hyperbole :> es
  => Eff es WH.Response
pageAbout = WH.view $ do
  canvas "Nothing" $ do
    WH.el (WH.bold . WH.fontSize 32) "About"


canvas :: WH.View c () -> WH.View c () -> WH.View c ()
canvas s x = WH.row WH.root $ do
  WH.col sideStyle $ do
    WH.link (WH.routeUrl AppRouteHome) logoStyle "postmap"
    s
    WH.space
    WH.link (WH.routeUrl AppRouteAbout) (WH.color Primary) "About"
  WH.col WH.grow x
  where
    logoStyle =
      WH.fontSize 32
        . WH.bold
        . WH.color Primary
        . WH.textAlign WH.Center
        . WH.border (WH.TRBL 0 0 1 0)
        . WH.borderColor Primary
    styBorderColor = WH.borderColor SecondaryLight
    sideStyle =
      WH.border (WH.TRBL 0 1 0 0)
        . styBorderColor
        . WH.pad 8
        . WH.gap (WH.PxRem 6)
        . WH.bg GrayLight
        . WH.color GrayDark
        . WH.fontSize 16
        . WH.scroll


data AppColor
  = White
  | Light
  | GrayLight
  | GrayDark
  | Dark
  | Success
  | Danger
  | Warning
  | Primary
  | PrimaryLight
  | Secondary
  | SecondaryLight
  deriving (Show, Read, Generic, WH.Param)


instance ToHttpApiData AppColor where
  toQueryParam = Z.Text.tshow


instance FromHttpApiData AppColor where
  parseQueryParam t = do
    case readMaybe (T.unpack t) of
      Nothing -> Left $ "Invalid AppColor: " <> t
      (Just c) -> pure c


instance WH.ToColor AppColor where
  colorValue White = "#FFF"
  colorValue Light = "#F2F2F3"
  colorValue GrayLight = "#E3E5E9"
  colorValue GrayDark = "#2C3C44"
  colorValue Dark = "#2E3842"
  colorValue Primary = "#4171b7"
  colorValue PrimaryLight = "#6D9BD3"
  colorValue Secondary = "#5D5A5C"
  colorValue SecondaryLight = "#9D999C"
  colorValue Success = "#149e5a"
  colorValue Danger = "#ef1509"
  colorValue Warning = "#e1c915"
