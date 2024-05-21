module Postmap.Spec (
  Spec (..),
  Record (..),
  Field (..),
  FieldReference (..),
  FieldName (..),
  mkFieldName,
  RecordName (..),
  mkRecordName,
  fromSchemaJson,
  fromSchema,
  emptySchema,
) where

import Postmap.Spec.FromSchema (fromSchema, fromSchemaJson)
import Postmap.Spec.Types (Field (..), FieldName (..), FieldReference (..), Record (..), RecordName (..), Spec (..), emptySchema, mkFieldName, mkRecordName)

