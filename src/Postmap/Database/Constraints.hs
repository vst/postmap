{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides definitions for PostgreSQL constraints.
module Postmap.Database.Constraints where

import qualified Autodocodec as ADC
import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
import GHC.Generics (Generic)
import Postmap.Database.Names
import Postmap.Database.Reference


-- * Unique Constraints


data UniqueConstraint
  = UniqueConstraintS ColumnName
  | UniqueConstraintC (NE.NonEmpty ColumnName)
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec UniqueConstraint)


instance ADC.HasCodec UniqueConstraint where
  codec =
    ADC.object "UniqueConstraint" ADC.objectCodec


instance ADC.HasObjectCodec UniqueConstraint where
  objectCodec =
    ADC.discriminatedUnionCodec "type" enc dec
    where
      codecS = ADC.requiredField "value" "Column for the single-column unique constraint."
      codecM = ADC.requiredField "value" "Columns for the composite unique constraint."
      enc x = case x of
        UniqueConstraintS s -> ("single", ADC.mapToEncoder s codecS)
        UniqueConstraintC c -> ("composite", ADC.mapToEncoder c codecM)
      dec =
        [ ("single", ("UniqueConstraintS", ADC.mapToDecoder UniqueConstraintS codecS))
        , ("composite", ("UniqueConstraintC", ADC.mapToDecoder UniqueConstraintC codecM))
        ]


isColumnUnique :: ColumnName -> UniqueConstraint -> Bool
isColumnUnique c uc = case uc of
  UniqueConstraintS s -> c == s
  UniqueConstraintC (s NE.:| []) -> c == s
  _ -> False


-- * Primary Key


data PrimaryKey
  = PrimaryKeyS ColumnName
  | PrimaryKeyC (NE.NonEmpty ColumnName)
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec PrimaryKey)


instance ADC.HasCodec PrimaryKey where
  codec =
    ADC.object "PrimaryKey" ADC.objectCodec


instance ADC.HasObjectCodec PrimaryKey where
  objectCodec =
    ADC.discriminatedUnionCodec "type" enc dec
    where
      codecS = ADC.requiredField "value" "Column for the single-column primary key."
      codecC = ADC.requiredField "value" "Columns for the composite primary key."
      enc x = case x of
        PrimaryKeyS s -> ("single", ADC.mapToEncoder s codecS)
        PrimaryKeyC m -> ("composite", ADC.mapToEncoder m codecC)
      dec =
        [ ("single", ("PrimaryKeyS", ADC.mapToDecoder PrimaryKeyS codecS))
        , ("composite", ("PrimaryKeyC", ADC.mapToDecoder PrimaryKeyC codecC))
        ]


isColumnPartOfPrimaryKey :: ColumnName -> PrimaryKey -> Bool
isColumnPartOfPrimaryKey c pk = case pk of
  PrimaryKeyS s -> c == s
  PrimaryKeyC cs -> c `elem` cs


-- * Foreign Key


newtype ForeignKey = MkForeignKey
  { foreignKeyColumns :: NE.NonEmpty ForeignKeyColumnPair
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ForeignKey)


instance ADC.HasCodec ForeignKey where
  codec =
    ADC.object "ForeignKey" $
      MkForeignKey
        <$> ADC.requiredField "columns" "List of pairs of column names forming the foreign key." ADC..= foreignKeyColumns


data ForeignKeyColumnPair = MkForeignKeyColumnPair
  { foreignKeyColumnPairSource :: !ColumnName
  , foreignKeyColumnPairTarget :: !ColumnReference
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ForeignKeyColumnPair)


instance ADC.HasCodec ForeignKeyColumnPair where
  codec =
    ADC.object "ForeignKeyColumnPair" $
      MkForeignKeyColumnPair
        <$> ADC.requiredField "source" "Name of the source column." ADC..= foreignKeyColumnPairSource
        <*> ADC.requiredField "target" "Name of the target column." ADC..= foreignKeyColumnPairTarget


findTargetReferenceForColumn :: ColumnName -> ForeignKey -> Maybe ColumnReference
findTargetReferenceForColumn c MkForeignKey {..} =
  findTargetReferenceForColumn' c (NE.toList foreignKeyColumns)


findTargetReferenceForColumn' :: ColumnName -> [ForeignKeyColumnPair] -> Maybe ColumnReference
findTargetReferenceForColumn' _ [] = Nothing
findTargetReferenceForColumn' c (x : xs)
  | c == foreignKeyColumnPairSource x = Just (foreignKeyColumnPairTarget x)
  | otherwise = findTargetReferenceForColumn' c xs
