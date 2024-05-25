{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Postmap.Gencode.Haskell where

import qualified Data.List as List
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String.Interpolate (i)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Postmap.Introspect (ColumnName (..), TableName (..), TableSchemaName (..))
import Postmap.Spec (Field (..), FieldName (..), FieldReference (..), Record (..), RecordName (..), Spec (..))
import qualified Text.Casing as Casing


data Config = Config
  { configDirectorySrc :: FilePath
  , configModuleName :: T.Text
  }
  deriving (Eq, Show)


configOutputDirectory :: Config -> FilePath
configOutputDirectory Config {..} =
  configDirectorySrc <> "/" <> T.unpack (T.intercalate "/" (T.split (== '.') configModuleName))


configOutputDirectoryRecords :: Config -> FilePath
configOutputDirectoryRecords config =
  configOutputDirectory config <> "/Records"


configOutputRecordModuleFile :: Config -> T.Text -> FilePath
configOutputRecordModuleFile config mn =
  configOutputDirectoryRecords config <> "/" <> T.unpack mn <> ".hs"


configOutputIdsModuleFile :: Config -> FilePath
configOutputIdsModuleFile config =
  configOutputDirectory config <> "/Identifiers.hs"


generateHaskell :: Config -> Spec -> IO ()
generateHaskell config@Config {..} Spec {..} = do
  recmods <- mapM (generateRecord config) specRecords
  TIO.writeFile (configOutputIdsModuleFile config) (mkIdsModule config specRecords)
  TIO.writeFile
    (configOutputDirectory config <> ".hs")
    [i|module #{configModuleName} (
  #{T.intercalate ",\n  " $ fmap ("module " <>) recmods}
) where

#{T.intercalate "\n" $ fmap ("import " <>) recmods}
|]


mkIdsModule :: Config -> [Record] -> T.Text
mkIdsModule Config {..} records =
  let ids = mapMaybe mkRecordId (List.nub records)
      modules' = mapMaybe fst ids
      modules =
        List.nub . List.sort $
          "Autodocodec"
            : "Data.Aeson"
            : "Data.Aeson"
            : "Data.Eq"
            : "Data.OpenApi"
            : "Data.OpenApi"
            : "GHC.Enum"
            : "Rel8"
            : "Servant"
            : "Text.Show"
            : modules'
   in [i|{-\# LANGUAGE DeriveAnyClass \#-}
{-\# LANGUAGE DeriveGeneric \#-}
{-\# LANGUAGE DerivingVia \#-}
{-\# LANGUAGE DuplicateRecordFields \#-}
{-\# LANGUAGE FlexibleInstances \#-}
{-\# LANGUAGE GeneralizedNewtypeDeriving \#-}
{-\# LANGUAGE NoImplicitPrelude \#-}
{-\# LANGUAGE OverloadedStrings \#-}
{-\# LANGUAGE RecordWildCards \#-}
{-\# LANGUAGE StandaloneDeriving \#-}
{-\# LANGUAGE TypeOperators \#-}

-- | This module provides for identifiers definitions for records.
module #{configModuleName}.Identifiers where

#{T.intercalate "\n" (fmap ("import qualified " <>) modules)}

#{T.intercalate "\n\n" (fmap snd ids)}
|]


mkRecordId :: Record -> Maybe (Maybe T.Text, T.Text)
mkRecordId Record {..} =
  case List.find fieldIsPrimaryKey recordFields of
    Nothing -> Nothing
    Just Field {..} ->
      Just $
        let tName = unRecordName recordName <> "Id"
            cName = "Mk" <> tName
            title = fromMaybe (unRecordName recordName) recordTitle
            tType = fromMaybe (defFieldType fieldColumnType) fieldType
            tModule = filterMaybe (not . T.null) . T.dropEnd 1 . T.dropWhileEnd (/= '.') $ tType
         in ( tModule
            , [i|-- | Identifier type for "#{title}" record.
newtype #{tName} = #{cName}
  { _un#{tName} :: #{tType}
  }
  deriving newtype (Rel8.DBEq, Rel8.DBType, GHC.Enum.Bounded, Data.Eq.Eq, Text.Show, Data.OpenApi.ToParamSchema, Servant.FromHttpApiData)
  deriving (Data.Aeson.FromJSON, Data.Aeson.ToJSON, Data.OpenApi.ToSchema) via (Autodocodec.Autodocodec #{tName})


instance Autodocodec.HasCodec #{tName} where
  codec = Autodocodec.named _type _codec Autodocodec.<?> _docs
    where
      _type = "#{tName}"
      _docs = "#{title} Identifier."
      _codec = Autodocodec.dimapCodec #{tName} _un#{tName} Autodocodec.codec
|]
            )


generateRecord :: Config -> Record -> IO T.Text
generateRecord config@Config {..} record =
  let (n, dt) = mkRecordDataType config record
   in do
        TIO.writeFile (configOutputRecordModuleFile config n) dt
        pure $ configModuleName <> ".Records." <> n


mkRecordDataType :: Config -> Record -> (T.Text, T.Text)
mkRecordDataType config@Config {..} record@Record {..} =
  let hkdName = mkRecordHkdTypeName record
      cnsName = mkRecordConstructorName record
      title = fromMaybe (unRecordName recordName) recordTitle
      table = [i|"#{unTableSchemaName recordTableSchema}"."#{unTableName recordTableName}"|] :: T.Text
      iFlds = fmap (mkRecordDataTypeField config record) recordFields
      fields = T.intercalate "\n  , " $ fmap snd iFlds
      modules'' = mapMaybe (filterMaybe (not . T.null) . T.dropEnd 1 . T.dropWhileEnd (/= '.') . fst) iFlds
      modules' = ["Data.Maybe" | not (all fieldNotNullable recordFields)] <> modules''
      modules =
        List.nub . List.sort $
          "Rel8"
            : "GHC.Generics"
            : "Data.Eq"
            : "Text.Show"
            : "Autodocodec"
            : "Data.Aeson"
            : "Data.OpenApi"
            : "Control.Applicative"
            : "Data.Functor"
            : modules'
      content =
        [i|{-\# LANGUAGE DeriveAnyClass \#-}
{-\# LANGUAGE DeriveGeneric \#-}
{-\# LANGUAGE DerivingVia \#-}
{-\# LANGUAGE DuplicateRecordFields \#-}
{-\# LANGUAGE FlexibleInstances \#-}
{-\# LANGUAGE GeneralizedNewtypeDeriving \#-}
{-\# LANGUAGE NoImplicitPrelude \#-}
{-\# LANGUAGE OverloadedStrings \#-}
{-\# LANGUAGE RecordWildCards \#-}
{-\# LANGUAGE StandaloneDeriving \#-}
{-\# LANGUAGE TypeOperators \#-}

-- | This module provides for /#{title}/ record definition, its database mapping and other related definitions.
module #{configModuleName}.Records.#{cnsName} where

#{T.intercalate "\n" (fmap ("import qualified " <>) modules)}


-- * Data Definition


-- | Data type representing the /#{title}/ record backed by database table @#{table}@.
data #{hkdName} f = #{cnsName}
  { #{fields}
  }
  deriving stock (GHC.Generics.Generic)
  deriving anyclass (Rel8.Rel8able)


deriving stock instance f ~ Rel8.Result => Data.Eq.Eq (#{hkdName} f)


deriving stock instance f ~ Rel8.Result => Text.Show.Show (#{hkdName} f)


-- | Result type definition for /#{title}/ record.
type #{cnsName} = #{hkdName} Rel8.Result


deriving via Autodocodec.Autodocodec #{cnsName} instance Data.Aeson.FromJSON #{cnsName}


deriving via Autodocodec.Autodocodec #{cnsName} instance Data.Aeson.ToJSON #{cnsName}


deriving via Autodocodec.Autodocodec #{cnsName} instance Data.OpenApi.ToSchema #{cnsName}


instance Autodocodec.HasCodec #{cnsName} where
  codec =
    Autodocodec.object "#{cnsName}" $
      #{cnsName}
        Data.Functor.<$> #{T.intercalate "\n        Control.Applicative.<*> " (fmap (mkRecordJsonField record) recordFields)}


-- * Database Mapping


-- | 'Rel8.TableSchema' definition for '#{hkdName}'.
table#{cnsName} :: Rel8.TableSchema (#{hkdName} Rel8.Name)
table#{cnsName} =
  Rel8.TableSchema
    { Rel8.name = "#{unTableName recordTableName}"
    , Rel8.columns =
        #{cnsName}
          { #{T.intercalate "\n          , " (fmap (mkRecordColMapping record) recordFields)}
          }
    }
|]
   in (cnsName, content)


mkRecordDataTypeField :: Config -> Record -> Field -> (T.Text, T.Text)
mkRecordDataTypeField Config {..} record@Record {..} field@Field {..} =
  let fName = mkRecordFieldName record field
      fType'
        | fieldIsPrimaryKey = configModuleName <> ".Identifiers." <> mkRecordIdTypeName recordName
        | otherwise = case fieldReference of
            Just FieldReference {..} -> mkRecordIdTypeName fieldReferenceRecord
            Nothing -> fromMaybe (defFieldType fieldColumnType) fieldType
      fType = if fieldNotNullable then fType' else [i|(Data.Maybe.Maybe #{fType'})|]
      fDesc = maybe "" (" -- ^ " <>) fieldDescription
   in (fType', [i|#{fName} :: !(Rel8.Column f #{fType})#{fDesc}|])


mkRecordJsonField :: Record -> Field -> T.Text
mkRecordJsonField record field@Field {..} =
  let jName = T.pack . Casing.snake . T.unpack $ unFieldName fieldName
      fName = mkRecordFieldName record field
      fDesc = fromMaybe "<undocumented>" fieldDescription
   in [i|Autodocodec.requiredField "#{jName}" "#{fDesc}" Autodocodec..= #{fName}|]


mkRecordColMapping :: Record -> Field -> T.Text
mkRecordColMapping record field@Field {..} =
  let fName = mkRecordFieldName record field
      fCol = unColumnName fieldColumnName
   in [i|#{fName} = "#{fCol}"|]


defFieldType :: T.Text -> T.Text
defFieldType "date" = "Data.Time.Day"
defFieldType "time" = "Data.Time.TimeOfDay"
defFieldType "timestamp" = "Data.Time.LocalTime"
defFieldType "timestamptz" = "Data.Time.UTCTime"
defFieldType "jsonb" = "Data.Aeson.Value"
defFieldType "json" = "Data.Aeson.Value"
defFieldType "uuid" = "Data.UUID.UUID"
defFieldType "text" = "Data.Text.Text"
defFieldType "varchar" = "Data.Text.Text"
defFieldType "int2" = "Data.Int.Int16"
defFieldType "int4" = "Data.Int.Int32"
defFieldType "int8" = "Data.Int.Int64"
defFieldType "float4" = "GHC.Float.Float"
defFieldType "float8" = "GHC.Float.Double"
defFieldType "numeric" = "Data.Scientific.Scientific"
defFieldType "bool" = "Data.Bool.Bool"
defFieldType "bytea" = "Data.ByteString.ByteString"
defFieldType "inet" = "Data.Text.Text"
defFieldType "cidr" = "Data.Text.Text"
defFieldType "macaddr" = "Data.Text.Text"
defFieldType "macaddr8" = "Data.Text.Text"
defFieldType "bit" = "Data.Text.Text"
defFieldType x = [i|<unknown database column type to map: #{x}>|]


mkRecordHkdTypeName :: Record -> T.Text
mkRecordHkdTypeName record =
  mkRecordConstructorName record <> "F"


mkRecordConstructorName :: Record -> T.Text
mkRecordConstructorName Record {..} =
  mkRecordConstructorName' recordName


mkRecordConstructorName' :: RecordName -> T.Text
mkRecordConstructorName' rn =
  unRecordName rn <> "Record"


mkRecordFieldName :: Record -> Field -> T.Text
mkRecordFieldName Record {..} Field {..} =
  mkRecordFieldName' recordName fieldName


mkRecordFieldName' :: RecordName -> FieldName -> T.Text
mkRecordFieldName' rn fn =
  lowerFirst (mkRecordConstructorName' rn) <> upperFirst (unFieldName fn)


mkRecordIdTypeName :: RecordName -> T.Text
mkRecordIdTypeName rn =
  mkRecordConstructorName' rn <> "Id"


lowerFirst :: T.Text -> T.Text
lowerFirst t =
  T.toLower (T.take 1 t) <> T.drop 1 t


upperFirst :: T.Text -> T.Text
upperFirst t =
  T.toUpper (T.take 1 t) <> T.drop 1 t


filterMaybe :: (a -> Bool) -> a -> Maybe a
filterMaybe p a
  | p a = Just a
  | otherwise = Nothing