{-# LANGUAGE RecordWildCards #-}

module Postmap.Spec.FromSchema where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Postmap.Introspect (Column (..), ColumnName (..), ColumnReference (..), Table (..), TableName (..))
import Postmap.Spec.Types (Field (..), FieldName (..), FieldReference (..), Record (..), RecordName (..), Spec (..))


fromSchemaJson :: BL.ByteString -> Either String Spec
fromSchemaJson = Aeson.eitherDecode


fromSchema :: [Table] -> Spec
fromSchema tables =
  Spec
    { specTitle = Nothing
    , specDescription = Nothing
    , specRecords = fmap fromTable tables
    }


fromTable :: Table -> Record
fromTable Table {..} =
  Record
    { recordName = mkRecordNameFromTableName tableName
    , recordTitle = Nothing
    , recordDescription = Nothing
    , recordTableSchema = tableSchema
    , recordTableName = tableName
    , recordFields = fmap fromColumn tableColumns
    , recordUniques = fmap (fmap mkFieldNameFromColumnName) tableUniques
    }


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


mkFieldNameFromColumnName :: ColumnName -> FieldName
mkFieldNameFromColumnName = MkFieldName . unColumnName


mkRecordNameFromTableName :: TableName -> RecordName
mkRecordNameFromTableName = MkRecordName . unTableName


mkFieldReferenceFromColumnReference :: ColumnReference -> FieldReference
mkFieldReferenceFromColumnReference ColumnReference {..} =
  FieldReference
    { fieldReferenceRecord = mkRecordNameFromTableName columnReferenceTable
    , fieldReferenceField = mkFieldNameFromColumnName columnReferenceColumn
    }
