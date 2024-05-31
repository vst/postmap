{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Postmap.Spec.Types where

import qualified Autodocodec as ADC
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Postmap.Introspect hiding (tokenParser)
import Text.Parsec (Parsec)
import qualified Text.Parsec as TP
import qualified Text.Parsec.Language
import qualified Text.Parsec.Token
import qualified Text.ParserCombinators.Parsec as TPC


-- $setup
--
-- >>> :set -XOverloadedStrings


data Spec = Spec
  { specTitle :: !(Maybe T.Text)
  , specDescription :: !(Maybe T.Text)
  , specRecords :: ![Record]
  }
  deriving stock (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Spec)


instance ADC.HasCodec Spec where
  codec =
    ADC.object "Spec" $
      Spec
        <$> ADC.optionalField "title" "Title of the specification." ADC..= specTitle
        <*> ADC.optionalField "description" "Description of the specification." ADC..= specDescription
        <*> ADC.requiredField "records" "Records in the specification." ADC..= specRecords


emptySchema :: Spec
emptySchema =
  Spec
    { specTitle = Nothing
    , specDescription = Nothing
    , specRecords = []
    }


data Record = Record
  { recordName :: !RecordName
  , recordTitle :: !(Maybe T.Text)
  , recordDescription :: !(Maybe T.Text)
  , recordTableSchema :: !TableSchemaName
  , recordTableName :: !TableName
  , recordFields :: ![Field]
  , recordUniques :: ![[FieldName]]
  , recordIsView :: !Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Record)


instance ADC.HasCodec Record where
  codec =
    ADC.object "Record" $
      Record
        <$> ADC.requiredField "name" "Name of the record." ADC..= recordName
        <*> ADC.optionalField "title" "Title of the record." ADC..= recordTitle
        <*> ADC.optionalField "description" "Description of the record." ADC..= recordDescription
        <*> ADC.requiredField "tableSchema" "Schema of the table." ADC..= recordTableSchema
        <*> ADC.requiredField "tableName" "Name of the table." ADC..= recordTableName
        <*> ADC.requiredField "fields" "Fields in the record." ADC..= recordFields
        <*> ADC.requiredField "uniques" "Unique constraints in the record." ADC..= recordUniques
        <*> ADC.requiredField "isView" "Whether the record is coming from a view." ADC..= recordIsView


newtype RecordName = MkRecordName
  { unRecordName :: T.Text
  }
  deriving stock (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec RecordName)


instance ADC.HasCodec RecordName where
  codec =
    ADC.bimapCodec mkRecordName unRecordName ADC.textCodec


-- | Smart constructor for 'RecordName'.
mkRecordName :: T.Text -> Either String RecordName
mkRecordName s =
  either (Left . show) (Right . MkRecordName . T.pack) (TP.parse parser "" (T.unpack s))
  where
    parser = Text.Parsec.Token.identifier tokenParser


data Field = Field
  { fieldName :: !FieldName
  , fieldType :: !(Maybe T.Text)
  , fieldColumnName :: !ColumnName
  , fieldColumnType :: !T.Text
  , fieldNotNullable :: !Bool
  , fieldIsPrimaryKey :: !Bool
  , fieldIsUnique :: !Bool
  , fieldReference :: !(Maybe FieldReference)
  , fieldDescription :: !(Maybe T.Text)
  }
  deriving stock (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Field)


instance ADC.HasCodec Field where
  codec =
    ADC.object "Field" $
      Field
        <$> ADC.requiredField "name" "Name of the field." ADC..= fieldName
        <*> ADC.requiredField "type" "Type of the field." ADC..= fieldType
        <*> ADC.requiredField "columnName" "Name of the column." ADC..= fieldColumnName
        <*> ADC.requiredField "columnType" "Type of the column." ADC..= fieldColumnType
        <*> ADC.requiredField "notNullable" "Whether the field is not nullable." ADC..= fieldNotNullable
        <*> ADC.requiredField "isPrimaryKey" "Whether the field is a primary key." ADC..= fieldIsPrimaryKey
        <*> ADC.requiredField "isUnique" "Whether the field is unique." ADC..= fieldIsUnique
        <*> ADC.optionalField "reference" "Reference to another field." ADC..= fieldReference
        <*> ADC.optionalField "description" "Description of the field." ADC..= fieldDescription


newtype FieldName = MkFieldName
  { unFieldName :: T.Text
  }
  deriving stock (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec FieldName)


instance ADC.HasCodec FieldName where
  codec =
    ADC.bimapCodec mkFieldName unFieldName ADC.textCodec


-- | Smart constructor for 'FieldName'.
mkFieldName :: T.Text -> Either String FieldName
mkFieldName s =
  either (Left . show) (Right . MkFieldName . T.pack) (TP.parse parser "" (T.unpack s))
  where
    parser = Text.Parsec.Token.identifier tokenParser


data FieldReference = FieldReference
  { fieldReferenceRecord :: !RecordName
  , fieldReferenceField :: !FieldName
  }
  deriving stock (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec FieldReference)


instance ADC.HasCodec FieldReference where
  codec =
    ADC.object "FieldReference" $
      FieldReference
        <$> ADC.requiredField "record" "Record being referenced." ADC..= fieldReferenceRecord
        <*> ADC.requiredField "field" "Field being referenced." ADC..= fieldReferenceField


-- ** Parsing


-- | Parse a lower-case alphabetic character.
--
-- >>> TP.parse lowerAlpha "" "a"
-- Right 'a'
-- >>> TP.parse lowerAlpha "" " "
-- Left (line 1, column 1):
-- unexpected " "
-- expecting lower-case alphabetic character
-- >>> TP.parse lowerAlpha "" "1"
-- Left (line 1, column 1):
-- unexpected "1"
-- expecting lower-case alphabetic character
-- >>> TP.parse lowerAlpha "" "A"
-- Left (line 1, column 1):
-- unexpected "A"
-- expecting lower-case alphabetic character
lowerAlpha :: Parsec T.Text () Char
lowerAlpha =
  TPC.satisfy ((&&) <$> Char.isAlpha <*> Char.isLower)
    TP.<?> "lower-case alphabetic character"


tokenParser :: Text.Parsec.Token.GenTokenParser String u Identity
tokenParser =
  Text.Parsec.Token.makeTokenParser Text.Parsec.Language.haskellStyle
