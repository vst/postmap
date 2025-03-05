{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Postmap.Codegen.Haskell.Overriding where

import qualified Autodocodec as ADC
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Postmap.Database as PD


data SchemaOverride = MkSchemaOverride
  { schemaOverrideName :: !PD.SchemaName
  , schemaOverrideTables :: ![TableOverride]
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec SchemaOverride)


instance ADC.HasCodec SchemaOverride where
  codec =
    ADC.object "SchemaOverride" $
      MkSchemaOverride
        <$> ADC.requiredField "name" "Name of the schema." ADC..= schemaOverrideName
        <*> ADC.requiredField "tables" "Overrides for the tables of the schema." ADC..= schemaOverrideTables


data TableOverride = MkTableOverride
  { tableOverrideName :: !PD.TableName
  , tableOverrideColumns :: ![TableColumnOverride]
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec TableOverride)


instance ADC.HasCodec TableOverride where
  codec =
    ADC.object "TableOverride" $
      MkTableOverride
        <$> ADC.requiredField "name" "Name of the table." ADC..= tableOverrideName
        <*> ADC.requiredField "columns" "Overrides for the columns of the table." ADC..= tableOverrideColumns


data TableColumnOverride = MkTableColumnOverride
  { tableColumnOverrideName :: !PD.ColumnName
  , tableColumnOverrideType :: !(Maybe T.Text)
  , tableColumnOverrideNullable :: !(Maybe Bool)
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec TableColumnOverride)


instance ADC.HasCodec TableColumnOverride where
  codec =
    ADC.object "TableColumnOverride" $
      MkTableColumnOverride
        <$> ADC.requiredField "name" "Name of the column." ADC..= tableColumnOverrideName
        <*> ADC.optionalField "type" "Override for type of the column." ADC..= tableColumnOverrideType
        <*> ADC.optionalField "nullable" "Whether the column is forced to be (non)nullable." ADC..= tableColumnOverrideNullable


findTableColumnOverride :: PD.TableName -> PD.ColumnName -> SchemaOverride -> Maybe TableColumnOverride
findTableColumnOverride tableName columnName MkSchemaOverride {..} =
  case filter (\MkTableOverride {..} -> tableName == tableOverrideName) schemaOverrideTables of
    [] -> Nothing
    [MkTableOverride {..}] ->
      case filter (\MkTableColumnOverride {..} -> columnName == tableColumnOverrideName) tableOverrideColumns of
        [] -> Nothing
        [columnOverride] -> Just columnOverride
        _ -> error "Multiple column overrides found."
    _ -> error "Multiple table overrides found."
