{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Postmap.Codegen.Haskell.Common where

import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Profunctor (dimap)
import qualified Data.Text as T
import Postmap.Codegen.Haskell.Overriding
import qualified Postmap.Database as PD
import qualified Text.Casing as Casing


data HaskellRecord = MkHaskellRecord
  { haskellRecordName :: !PD.TableName
  , haskellRecordFields :: ![HaskellRecordField]
  }


data HaskellRecordField = MkHaskellRecordField
  { haskellRecordFieldName :: !PD.ColumnName
  , haskellRecordFieldType :: !HaskellRecordFieldType
  , haskellRecordFieldNullable :: !Bool
  }
  deriving (Show)


data HaskellRecordFieldType
  = MkHaskellRecordFieldTypePgType !(Bool, PD.PgType)
  | MkHaskellRecordFieldTypePrimaryKey !(Bool, PD.PgType, PD.ColumnReference)
  | MkHaskellRecordFieldTypeForeignKey !(Bool, PD.ColumnReference)
  | MkHaskellRecordFieldTypeCustom !(Bool, T.Text)
  deriving (Show)


compileHaskellRecords :: SchemaOverride -> PD.Schema -> [HaskellRecord]
compileHaskellRecords override schema@PD.MkSchema {..} =
  let recordModules = fmap (compileHaskellRecord override schema) schemaTables
   in recordModules


compileHaskellRecord :: SchemaOverride -> PD.Schema -> PD.Table -> HaskellRecord
compileHaskellRecord override schema table@PD.MkTable {..} =
  MkHaskellRecord
    { haskellRecordName = tableName
    , haskellRecordFields = fmap (compileHaskellRecordField override schema table) tableColumns
    }


compileHaskellRecordField :: SchemaOverride -> PD.Schema -> PD.Table -> PD.Column -> HaskellRecordField
compileHaskellRecordField override schema table@PD.MkTable {..} column@PD.MkColumn {..} =
  let columnOverride = findTableColumnOverride tableName columnName override
      columnTypeOverride = tableColumnOverrideType =<< columnOverride
      columnNullableOverride = tableColumnOverrideNullable =<< columnOverride
   in MkHaskellRecordField
        { haskellRecordFieldName = columnName
        , haskellRecordFieldType = case columnTypeOverride of
            Nothing -> compileHaskellRecordFieldType schema table column
            Just t -> MkHaskellRecordFieldTypeCustom (columnNullable, t)
        , haskellRecordFieldNullable = fromMaybe columnNullable columnNullableOverride
        }


compileHaskellRecordFieldType :: PD.Schema -> PD.Table -> PD.Column -> HaskellRecordFieldType
compileHaskellRecordFieldType schema table column@PD.MkColumn {..} =
  case resolveHaskellRecordFieldType schema table column of
    Just fieldType -> fieldType
    Nothing -> MkHaskellRecordFieldTypePgType (columnNullable, columnType)


resolveHaskellRecordFieldType :: PD.Schema -> PD.Table -> PD.Column -> Maybe HaskellRecordFieldType
resolveHaskellRecordFieldType PD.MkSchema {..} PD.MkTable {..} PD.MkColumn {..} =
  case findColumnReference columnName tableForeignKeys of
    Just cr -> Just (MkHaskellRecordFieldTypeForeignKey (columnNullable, cr))
    Nothing -> case tablePrimaryKey of
      Just (PD.PrimaryKeyS primaryKeyColumn) ->
        if columnName == primaryKeyColumn
          then
            Just
              ( MkHaskellRecordFieldTypePrimaryKey
                  ( columnNullable
                  , columnType
                  , PD.MkColumnReference schemaName tableName columnName
                  )
              )
          else Nothing
      _ -> Nothing


findColumnReference :: PD.ColumnName -> [PD.ForeignKey] -> Maybe PD.ColumnReference
findColumnReference _ [] = Nothing
findColumnReference columnName (fk : fks) =
  case findColumnReference' columnName fk of
    Just cr -> Just cr
    Nothing -> findColumnReference columnName fks


findColumnReference' :: PD.ColumnName -> PD.ForeignKey -> Maybe PD.ColumnReference
findColumnReference' columnName PD.MkForeignKey {..} =
  case foreignKeyColumns of
    a NE.:| [] | columnName == PD.foreignKeyColumnPairSource a -> Just (PD.foreignKeyColumnPairTarget a)
    _ -> Nothing


-- * Utilities


data Casing
  = CasingCamelCase
  | CasingSnakeCase
  | CasingPascalCase


-- | Converts the casing of a text value.
--
-- >>> :set -XOverloadedStrings
-- >>> caseText CasingCamelCase "camelCase"
-- "camelCase"
-- >>> caseText CasingCamelCase "snake_case"
-- "snakeCase"
-- >>> caseText CasingCamelCase "PascalCase"
-- "pascalCase"
-- >>> caseText CasingSnakeCase "camelCase"
-- "camel_case"
-- >>> caseText CasingSnakeCase "snake_case"
-- "snake_case"
-- >>> caseText CasingSnakeCase "PascalCase"
-- "pascal_case"
-- >>> caseText CasingPascalCase "camelCase"
-- "CamelCase"
-- >>> caseText CasingPascalCase "snake_case"
-- "SnakeCase"
-- >>> caseText CasingPascalCase "PascalCase"
-- "PascalCase"
caseText :: Casing -> (T.Text -> T.Text)
caseText = \case
  CasingCamelCase -> dimap T.unpack T.pack Casing.camel
  CasingSnakeCase -> dimap T.unpack T.pack Casing.quietSnake
  CasingPascalCase -> dimap T.unpack T.pack Casing.pascal


caseIdentifier :: Casing -> (PD.Identifier -> PD.Identifier)
caseIdentifier c =
  dimap PD._unIdentifier PD.MkIdentifier (caseText c)


caseSchemaName :: Casing -> (PD.SchemaName -> PD.SchemaName)
caseSchemaName c =
  dimap PD._unSchemaName PD.MkSchemaName (caseIdentifier c)


caseTableName :: Casing -> (PD.TableName -> PD.TableName)
caseTableName c =
  dimap PD._unTableName PD.MkTableName (caseIdentifier c)


caseColumnName :: Casing -> (PD.ColumnName -> PD.ColumnName)
caseColumnName c =
  dimap PD._unColumnName PD.MkColumnName (caseIdentifier c)


-- | Converts a 'PD.TableName' to a 'T.Text' value representing a Haskell data type.
--
-- >>> :set -XOverloadedStrings
-- >>> tableNameToDataName (PD.MkTableName (PD.MkIdentifier "foo_bar"))
-- "FooBar"
-- >>> tableNameToDataName (PD.MkTableName (PD.MkIdentifier "fooBar"))
-- "FooBar"
-- >>> tableNameToDataName (PD.MkTableName (PD.MkIdentifier "FooBar"))
-- "FooBar"
tableNameToDataName :: PD.TableName -> T.Text
tableNameToDataName =
  PD.tableNameToText . caseTableName CasingPascalCase


-- | Converts a 'PD.ColumnName' to a 'T.Text' value representing a Haskell record field.
--
-- >>> :set -XOverloadedStrings
-- >>> columnNameToFieldName (PD.MkColumnName (PD.MkIdentifier "foo_bar"))
-- "fooBar"
-- >>> columnNameToFieldName (PD.MkColumnName (PD.MkIdentifier "fooBar"))
-- "fooBar"
-- >>> columnNameToFieldName (PD.MkColumnName (PD.MkIdentifier "FooBar"))
-- "fooBar"
columnNameToFieldName :: PD.ColumnName -> T.Text
columnNameToFieldName =
  PD.columnNameToText . caseColumnName CasingCamelCase
