{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Postmap.Database.Names where

import qualified Autodocodec as ADC
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Text.Parsec
import qualified Text.Parsec.Language
import qualified Text.Parsec.Token
import qualified Zamazingo.Aeson as Z.Aeson


-- * Identifier


newtype Identifier = MkIdentifier
  { _unIdentifier :: T.Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Identifier)


instance Aeson.FromJSONKey Identifier where
  fromJSONKey =
    Z.Aeson.keyParserFromEitherText "Identifier" mkIdentifier


instance Aeson.ToJSONKey Identifier where
  toJSONKey =
    Aeson.Types.toJSONKeyText identifierToText


instance ADC.HasCodec Identifier where
  codec =
    ADC.bimapCodec mkIdentifier identifierToText ADC.textCodec


mkIdentifier :: T.Text -> Either String Identifier
mkIdentifier s =
  either (Left . show) (Right . MkIdentifier . T.pack) (Text.Parsec.parse parser "" (T.unpack s))
  where
    parser = Text.Parsec.Token.identifier identifierTokenParser


identifierToText :: Identifier -> T.Text
identifierToText =
  _unIdentifier


-- | Token parser for identifiers.
--
-- See <https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS>
--
-- Notes:
--
-- Maybe we should consider non-quoted identifiers. It would be simpler.
-- If we want to support non-quoted identifiers, we should consider the following:
--
-- @@
-- data Ident
--   = IdentNormal T.Text
--   | IdentQuoted T.Text
-- @@
identifierTokenParser :: Text.Parsec.Token.GenTokenParser String u Identity
identifierTokenParser =
  Text.Parsec.Token.makeTokenParser Text.Parsec.Language.haskellStyle


-- * Database Name


newtype DatabaseName = MkDatabaseName
  { _unDatabaseName :: Identifier
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec DatabaseName)


instance Aeson.FromJSONKey DatabaseName where
  fromJSONKey =
    Z.Aeson.keyParserFromEitherText "DatabaseName" mkDatabaseName


instance Aeson.ToJSONKey DatabaseName where
  toJSONKey =
    Aeson.Types.toJSONKeyText databaseNameToText


instance ADC.HasCodec DatabaseName where
  codec =
    ADC.bimapCodec mkDatabaseName databaseNameToText ADC.textCodec


mkDatabaseName :: T.Text -> Either String DatabaseName
mkDatabaseName =
  either (Left . show) (Right . MkDatabaseName) . mkIdentifier


databaseNameToText :: DatabaseName -> T.Text
databaseNameToText =
  identifierToText . _unDatabaseName


-- * Schema Name


newtype SchemaName = MkSchemaName
  { _unSchemaName :: Identifier
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec SchemaName)


instance Aeson.FromJSONKey SchemaName where
  fromJSONKey =
    Z.Aeson.keyParserFromEitherText "TableSchemaName" mkSchemaName


instance Aeson.ToJSONKey SchemaName where
  toJSONKey =
    Aeson.Types.toJSONKeyText schemaNameToText


instance ADC.HasCodec SchemaName where
  codec =
    ADC.bimapCodec mkSchemaName schemaNameToText ADC.textCodec


mkSchemaName :: T.Text -> Either String SchemaName
mkSchemaName =
  either (Left . show) (Right . MkSchemaName) . mkIdentifier


schemaNameToText :: SchemaName -> T.Text
schemaNameToText =
  identifierToText . _unSchemaName


-- * Table Name


newtype TableName = MkTableName
  { _unTableName :: Identifier
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec TableName)


instance Aeson.FromJSONKey TableName where
  fromJSONKey =
    Z.Aeson.keyParserFromEitherText "TableName" mkTableName


instance Aeson.ToJSONKey TableName where
  toJSONKey =
    Aeson.Types.toJSONKeyText tableNameToText


instance ADC.HasCodec TableName where
  codec =
    ADC.bimapCodec mkTableName tableNameToText ADC.textCodec


mkTableName :: T.Text -> Either String TableName
mkTableName =
  either (Left . show) (Right . MkTableName) . mkIdentifier


tableNameToText :: TableName -> T.Text
tableNameToText =
  identifierToText . _unTableName


-- * Column Name


newtype ColumnName = MkColumnName
  { _unColumnName :: Identifier
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ColumnName)


instance Aeson.FromJSONKey ColumnName where
  fromJSONKey =
    Z.Aeson.keyParserFromEitherText "ColumnName" mkColumnName


instance Aeson.ToJSONKey ColumnName where
  toJSONKey =
    Aeson.Types.toJSONKeyText columnNameToText


instance ADC.HasCodec ColumnName where
  codec =
    ADC.bimapCodec mkColumnName columnNameToText ADC.textCodec


mkColumnName :: T.Text -> Either String ColumnName
mkColumnName =
  either (Left . show) (Right . MkColumnName) . mkIdentifier


columnNameToText :: ColumnName -> T.Text
columnNameToText =
  identifierToText . _unColumnName
