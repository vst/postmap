{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Postmap.Diagrams where

import Data.Maybe (isJust, mapMaybe)
import Data.String.Interpolate (i)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Postmap.Spec (Field (..), FieldName (..), FieldReference (..), Record (..), RecordName (..), Spec (..))
import qualified System.Process.Typed as TP


runDiagrams :: FilePath -> Spec -> IO ()
runDiagrams path Spec {..} = do
  runDiagramMaster path specRecords
  mapM_ (runDiagramPerRecord path) specRecords


runDiagramMaster :: FilePath -> [Record] -> IO ()
runDiagramMaster path rs =
  let d2Rec = fmap renderSqlRecord rs
      d2Ref = fmap (\r@Record {..} -> T.intercalate "\n" $ mapMaybe (\f@Field {..} -> fmap (sqlRef False r f) fieldReference) recordFields) rs
      d2Sch = T.intercalate "\n" d2Rec <> "\n" <> T.intercalate "\n" d2Ref
      opDsl = path <> "/_schema.d2"
      opSvg = path <> "/_schema.svg"
   in do
        TIO.writeFile opDsl d2Sch
        TP.runProcess_ $ TP.proc "d2" ["--layout=elk", opDsl, opSvg]


runDiagramPerRecord :: FilePath -> Record -> IO ()
runDiagramPerRecord path record@Record {..} =
  let d2 = renderRecordD2 record
      opD2 = path <> "/" <> T.unpack (unRecordName recordName) <> ".d2"
      opSvg = path <> "/" <> T.unpack (unRecordName recordName) <> ".svg"
   in do
        TIO.writeFile opD2 d2
        TP.runProcess_ $ TP.proc "d2" ["--layout=elk", opD2, opSvg]


renderRecordD2 :: Record -> T.Text
renderRecordD2 record@Record {..} =
  let d2Rec = renderSqlRecord record
      d2Ref = T.intercalate "\n" $ mapMaybe (\f@Field {..} -> fmap (sqlRef True record f) fieldReference) recordFields
      d2Ent =
        T.intercalate "\n"
          . fmap (<> ".class: record")
          . filter (/= sqlRecordName recordName)
          $ mapMaybe (\Field {..} -> sqlRecordName . fieldReferenceRecord <$> fieldReference) recordFields
   in [i|
classes: {
  record: {
    style: {
      border-radius: 8
      font-size: 24
      bold: false
    }
  }
}

#{d2Ent}

#{d2Rec}

#{d2Ref}
|]


-- * Sql Record


renderSqlRecord :: Record -> T.Text
renderSqlRecord Record {..} =
  let fields = fmap renderSqlTableField recordFields
   in [i|#{sqlRecordName recordName} {
  shape: sql_table
  #{T.intercalate "\n  " fields}
}|]


-- * Sql Fields


renderSqlTableField :: Field -> T.Text
renderSqlTableField field@Field {..} =
  let constraints = T.intercalate ";" (sqlFieldConstraints field)
   in [i|#{sqlFieldName fieldName}: #{fieldType} {constraint: [#{constraints}]}|]


sqlFieldConstraints :: Field -> [T.Text]
sqlFieldConstraints Field {..} =
  let pk = (["primary_key" | fieldIsPrimaryKey])
      uq = (["unique" | fieldIsUnique])
      fk = (["foreign_key" | isJust fieldReference])
   in pk <> uq <> fk


-- * SQL References


sqlRef :: Bool -> Record -> Field -> FieldReference -> T.Text
sqlRef simple record field FieldReference {..} =
  let fr = sqlRecordNameFromRecord record
      ff = sqlFieldNameFromField field
      tr = sqlRecordName fieldReferenceRecord
      tf = sqlFieldName fieldReferenceField
   in if simple
        then [i|#{fr}.#{ff} -> #{tr}|]
        else [i|#{fr}.#{ff} -> #{tr}.#{tf}|]


-- * Helpers


sqlFieldName :: FieldName -> T.Text
sqlFieldName f =
  let fn = unFieldName f
   in fn <> if fn `elem` _reserved then "_" else ""


sqlFieldNameFromField :: Field -> T.Text
sqlFieldNameFromField Field {..} =
  sqlFieldName fieldName


sqlRecordName :: RecordName -> T.Text
sqlRecordName =
  unRecordName


sqlRecordNameFromRecord :: Record -> T.Text
sqlRecordNameFromRecord Record {..} =
  sqlRecordName recordName


_reserved :: [T.Text]
_reserved =
  [ "class"
  , "classes"
  , "constraint"
  , "direction"
  , "grid-columns"
  , "grid-gap"
  , "grid-rows"
  , "height"
  , "horizontal-gap"
  , "icon"
  , "label"
  , "left"
  , "link"
  , "near"
  , "shape"
  , "style"
  , "tooltip"
  , "top"
  , "vertical-gap"
  , "width"
  ]
