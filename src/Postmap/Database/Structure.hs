{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines entities describing a given PostgreSQL
-- database structure.
module Postmap.Database.Structure where

import qualified Autodocodec as ADC
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import Postmap.Database.Constraints
import Postmap.Database.Names
import Postmap.Database.Types


-- * Database


data Database = MkDatabase
  { databaseName :: !DatabaseName
  , databaseSchema :: !Schema
  }
  deriving stock (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Database)


instance ADC.HasCodec Database where
  codec =
    ADC.object "Database" $
      MkDatabase
        <$> ADC.requiredField "name" "Name of the database." ADC..= databaseName
        <*> ADC.requiredField "schema" "Database schema of interest." ADC..= databaseSchema


-- * Schema


data Schema = MkSchema
  { schemaName :: !SchemaName
  , schemaTables :: ![Table]
  }
  deriving stock (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Schema)


instance ADC.HasCodec Schema where
  codec =
    ADC.object "Schema" $
      MkSchema
        <$> ADC.requiredField "name" "Name of the schema." ADC..= schemaName
        <*> ADC.requiredField "tables" "Tables of the schema." ADC..= schemaTables


-- * Table


-- | A table in a PostgreSQL database.
data Table = MkTable
  { tableName :: !TableName
  , tableIsView :: !Bool
  , tableColumns :: ![Column]
  , tablePrimaryKey :: !(Maybe PrimaryKey)
  , tableForeignKeys :: ![ForeignKey]
  , tableUniqueConstraints :: ![UniqueConstraint]
  }
  deriving stock (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Table)


instance ADC.HasCodec Table where
  codec =
    ADC.object "Table" $
      MkTable
        <$> ADC.requiredField "name" "Name of the table." ADC..= tableName
        <*> ADC.requiredField "is_view" "Whether the table is a view." ADC..= tableIsView
        <*> ADC.requiredField "columns" "Columns of the table." ADC..= tableColumns
        <*> ADC.optionalField "primary_key" "Primary key of the table." ADC..= tablePrimaryKey
        <*> ADC.requiredField "foreign_keys" "Foreign keys of the table." ADC..= tableForeignKeys
        <*> ADC.requiredField "unique_constraints" "Unique constraints of the table." ADC..= tableUniqueConstraints


-- * Column


data Column = MkColumn
  { columnName :: !ColumnName
  , columnType :: !PgType
  , columnNullable :: !Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Column)


instance ADC.HasCodec Column where
  codec =
    ADC.object "Column" $
      MkColumn
        <$> ADC.requiredField "name" "Name of the column." ADC..= columnName
        <*> ADC.requiredField "type" "Type of the column." ADC..= columnType
        <*> ADC.requiredField "nullable" "Whether the column is nullable." ADC..= columnNullable
