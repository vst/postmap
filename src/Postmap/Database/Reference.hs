{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Postmap.Database.Reference where

import qualified Autodocodec as ADC
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import Postmap.Database.Names (ColumnName, SchemaName, TableName)


-- * Table Reference


data TableReference = MkTableReference
  { tableReferenceSchema :: !SchemaName
  , tableReferenceTable :: !TableName
  }
  deriving stock (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec TableReference)


instance ADC.HasCodec TableReference where
  codec =
    ADC.object "TableReference" $
      MkTableReference
        <$> ADC.requiredField "schema" "Name of the schema of the table." ADC..= tableReferenceSchema
        <*> ADC.requiredField "table" "Name of the table." ADC..= tableReferenceTable


-- * Column Reference


data ColumnReference = MkColumnReference
  { columnReferenceSchema :: !SchemaName
  , columnReferenceTable :: !TableName
  , columnReferenceColumn :: !ColumnName
  }
  deriving stock (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ColumnReference)


instance ADC.HasCodec ColumnReference where
  codec =
    ADC.object "ColumnReference" $
      MkColumnReference
        <$> ADC.requiredField "schema" "Name of the schema of the table of the column." ADC..= columnReferenceSchema
        <*> ADC.requiredField "table" "Name of the table of the column." ADC..= columnReferenceTable
        <*> ADC.requiredField "column" "Name of the column." ADC..= columnReferenceColumn
