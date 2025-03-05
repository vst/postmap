{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Postmap.Database.Types where

import qualified Autodocodec as ADC
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import GHC.Generics (Generic)


data PgType
  = PgTypeBit
  | PgTypeBool
  | PgTypeBpchar
  | PgTypeBytea
  | PgTypeCidr
  | PgTypeDate
  | PgTypeFloat4
  | PgTypeFloat8
  | PgTypeInet
  | PgTypeInt2
  | PgTypeInt4
  | PgTypeInt8
  | PgTypeJson
  | PgTypeJsonb
  | PgTypeMacaddr
  | PgTypeMacaddr8
  | PgTypeNumeric
  | PgTypeText
  | PgTypeTime
  | PgTypeTimestamp
  | PgTypeTimestamptz
  | PgTypeUuid
  | PgTypeVarchar
  | PgTypeArray PgType
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec PgType)


instance ADC.HasCodec PgType where
  codec =
    ADC.bimapCodec mkPgType pgTypeToText ADC.textCodec


-- | Create a 'PgType' from a 'T.Text'.
--
-- >>> :set -XOverloadedStrings
-- >>> mkPgType "int4"
-- Right PgTypeInt4
-- >>> mkPgType "int4[]"
-- Right (PgTypeArray PgTypeInt4)
-- >>> mkPgType "int4[][]"
-- Right (PgTypeArray (PgTypeArray PgTypeInt4))
mkPgType :: T.Text -> Either String PgType
mkPgType "bit" = Right PgTypeBit
mkPgType "bool" = Right PgTypeBool
mkPgType "bpchar" = Right PgTypeBpchar
mkPgType "bytea" = Right PgTypeBytea
mkPgType "cidr" = Right PgTypeCidr
mkPgType "date" = Right PgTypeDate
mkPgType "float4" = Right PgTypeFloat4
mkPgType "float8" = Right PgTypeFloat8
mkPgType "inet" = Right PgTypeInet
mkPgType "int2" = Right PgTypeInt2
mkPgType "int4" = Right PgTypeInt4
mkPgType "int8" = Right PgTypeInt8
mkPgType "json" = Right PgTypeJson
mkPgType "jsonb" = Right PgTypeJsonb
mkPgType "macaddr" = Right PgTypeMacaddr
mkPgType "macaddr8" = Right PgTypeMacaddr8
mkPgType "numeric" = Right PgTypeNumeric
mkPgType "text" = Right PgTypeText
mkPgType "time" = Right PgTypeTime
mkPgType "timestamp" = Right PgTypeTimestamp
mkPgType "timestamptz" = Right PgTypeTimestamptz
mkPgType "uuid" = Right PgTypeUuid
mkPgType "varchar" = Right PgTypeVarchar
mkPgType x
  | T.isSuffixOf "[]" x = PgTypeArray <$> mkPgType (T.dropEnd 2 x)
  | otherwise = Left $ "Unknown PgType: " <> T.unpack x


-- | Convert a 'PgType' to a 'T.Text'.
--
-- __Note About Arrays:__
--
-- Although CREATE TABLE statements can contain multi-dimensional
-- arrays, PostgreSQL does not enforce the number of dimensions. Our
-- introspection machinery will only return one-dimensional arrays.
-- Likewise, the array elements can be nullable.
--
-- This data type will in reality result only in one-dimensional arrays
-- with non-nullable elements.
--
-- >>> :set -XOverloadedStrings
-- >>> pgTypeToText PgTypeInt4
-- "int4"
-- >>> pgTypeToText (PgTypeArray PgTypeInt4)
-- "int4[]"
-- >>> pgTypeToText (PgTypeArray (PgTypeArray PgTypeInt4))
-- "int4[][]"
pgTypeToText :: PgType -> T.Text
pgTypeToText PgTypeBit = "bit"
pgTypeToText PgTypeBool = "bool"
pgTypeToText PgTypeBpchar = "bpchar"
pgTypeToText PgTypeBytea = "bytea"
pgTypeToText PgTypeCidr = "cidr"
pgTypeToText PgTypeDate = "date"
pgTypeToText PgTypeFloat4 = "float4"
pgTypeToText PgTypeFloat8 = "float8"
pgTypeToText PgTypeInet = "inet"
pgTypeToText PgTypeInt2 = "int2"
pgTypeToText PgTypeInt4 = "int4"
pgTypeToText PgTypeInt8 = "int8"
pgTypeToText PgTypeJson = "json"
pgTypeToText PgTypeJsonb = "jsonb"
pgTypeToText PgTypeMacaddr = "macaddr"
pgTypeToText PgTypeMacaddr8 = "macaddr8"
pgTypeToText PgTypeNumeric = "numeric"
pgTypeToText PgTypeText = "text"
pgTypeToText PgTypeTime = "time"
pgTypeToText PgTypeTimestamp = "timestamp"
pgTypeToText PgTypeTimestamptz = "timestamptz"
pgTypeToText PgTypeUuid = "uuid"
pgTypeToText PgTypeVarchar = "varchar"
pgTypeToText (PgTypeArray c) = pgTypeToText c <> "[]"
