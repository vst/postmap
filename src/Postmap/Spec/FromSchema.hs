{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Postmap.Spec.FromSchema where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as List
import qualified Data.Text as T
import Postmap.Introspect (Column (..), ColumnName (..), ColumnReference (..), Table (..), TableName (..))
import Postmap.Spec.Types (Field (..), FieldName (..), FieldReference (..), Record (..), RecordName (..), Spec (..))
import qualified Text.Casing as Casing


fromSchemaJson :: BL.ByteString -> Either String Spec
fromSchemaJson = Aeson.eitherDecode


fromSchema :: [ColumnName] -> [Table] -> Spec
fromSchema ordering tables =
  Spec
    { specTitle = Nothing
    , specDescription = Nothing
    , specRecords = fmap (fromTable ordering) tables
    }


fromTable :: [ColumnName] -> Table -> Record
fromTable ordering Table {..} =
  Record
    { recordName = mkRecordNameFromTableName tableName
    , recordTitle = Nothing
    , recordDescription = Nothing
    , recordTableSchema = tableSchema
    , recordTableName = tableName
    , recordFields = orderFields ordering (fmap fromColumn tableColumns)
    , recordUniques = fmap (fmap mkFieldNameFromColumnName) tableUniques
    }


data ColOrder
  = ColOrderProvided Int ColumnName
  | ColOrderDefault ColumnName
  deriving (Eq, Ord, Show)


mkColOrder :: [ColumnName] -> ColumnName -> ColOrder
mkColOrder ordering c =
  case List.elemIndex c ordering of
    Just i -> ColOrderProvided i c
    Nothing -> ColOrderDefault c


orderFields :: [ColumnName] -> [Field] -> [Field]
orderFields ordering fields =
  let marked = fmap (\f@Field {..} -> (mkColOrder ordering fieldColumnName, f)) fields
   in snd <$> List.sortOn fst marked


fromColumn :: Column -> Field
fromColumn Column {..} =
  Field
    { fieldName = mkFieldNameFromColumnName columnName
    , fieldType = columnType
    , fieldColumnName = columnName
    , fieldColumnType = columnType
    , fieldNotNullable = columnNullable
    , fieldIsPrimaryKey = columnIsPrimaryKey
    , fieldIsUnique = columnUnique
    , fieldReference = fmap mkFieldReferenceFromColumnReference columnForeignKey
    , fieldDescription = Nothing
    }


mkRecordNameFromTableName :: TableName -> RecordName
mkRecordNameFromTableName =
  MkRecordName . T.pack . Casing.pascal . T.unpack . unTableName


mkFieldNameFromColumnName :: ColumnName -> FieldName
mkFieldNameFromColumnName =
  MkFieldName . T.pack . Casing.camel . T.unpack . unColumnName


mkFieldReferenceFromColumnReference :: ColumnReference -> FieldReference
mkFieldReferenceFromColumnReference ColumnReference {..} =
  FieldReference
    { fieldReferenceRecord = mkRecordNameFromTableName columnReferenceTable
    , fieldReferenceField = mkFieldNameFromColumnName columnReferenceColumn
    }


-- * Tests


testColumnNames :: [ColumnName]
testColumnNames =
  [ MkColumnName "id"
  , MkColumnName "name"
  , MkColumnName "email"
  , MkColumnName "created_at"
  , MkColumnName "updated_at"
  ]


testPreferredOrder :: [ColOrder]
testPreferredOrder =
  let preffered = [MkColumnName "id", MkColumnName "created_at", MkColumnName "updated_at"]
   in fmap (mkColOrder preffered) testColumnNames


-- >>> testColumnNamesOrdered
-- [MkColumnName {unColumnName = "id"},MkColumnName {unColumnName = "created_at"},MkColumnName {unColumnName = "updated_at"},MkColumnName {unColumnName = "email"},MkColumnName {unColumnName = "name"}]
testColumnNamesOrdered :: [ColumnName]
testColumnNamesOrdered = snd <$> List.sortOn fst (zip testPreferredOrder testColumnNames)
