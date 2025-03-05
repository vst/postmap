{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Postmap.Codegen.Haskell.Rel8 where

import qualified Autodocodec as ADC
import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.List as L
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String.Interpolate (i)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import Postmap.Codegen.Haskell.Common
import Postmap.Codegen.Haskell.Overriding
import qualified Postmap.Database as PD
import System.Process.Typed (ExitCode (..))
import qualified System.Process.Typed as TP


-- * Configuration


data Config = MkConfig
  { configDirectory :: !FilePath
  , configModuleName :: !T.Text
  , configOverrides :: ![SchemaOverride]
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Config)


instance ADC.HasCodec Config where
  codec =
    ADC.object "Config" $
      MkConfig
        <$> ADC.requiredField "directory" "Directory where the generated Haskell code will be written." ADC..= configDirectory
        <*> ADC.requiredField "module" "Name of the top module to be generated." ADC..= configModuleName
        <*> ADC.requiredField "overrides" "Overrides for the schema, tables and columns." ADC..= configOverrides


configDirectoryModule :: Config -> FilePath
configDirectoryModule MkConfig {..} =
  configDirectory <> "/" <> T.unpack (T.intercalate "/" (T.split (== '.') configModuleName))


configDirectoryModuleIdentifiers :: Config -> FilePath
configDirectoryModuleIdentifiers config =
  configDirectoryModule config <> "/Identifiers.hs"


configDirectoryModuleRecords :: Config -> FilePath
configDirectoryModuleRecords config =
  configDirectoryModule config <> "/Records"


mkConfigDirectoryModuleRecord :: Config -> T.Text -> FilePath
mkConfigDirectoryModuleRecord config mn =
  configDirectoryModuleRecords config <> "/" <> T.unpack mn <> ".hs"


-- * Code Generation


-- | Generate Haskell code for the given database.
--
-- __WARNING__: This function works on single-schema databases only.
--
-- The idea is to create, for a given single-schema only database:
--
-- 1. One module for all record identifiers for single-column primary keys.
-- 2. One module for each record type that corresponds to a table in the database.
--
-- @@
-- /ModuleName/Identifiers.hs
-- /ModuleName/Records/RecordName1.hs
-- /ModuleName/Records/RecordName2.hs
-- @@
generate :: Config -> PD.Database -> IO ()
generate cfg@MkConfig {..} db = do
  let schema = PD.databaseSchema db
  let schemaOverride =
        fromMaybe
          MkSchemaOverride
            { schemaOverrideName = PD.schemaName schema
            , schemaOverrideTables = []
            }
          ( L.find
              ( \MkSchemaOverride {..} ->
                  schemaOverrideName == PD.schemaName schema
              )
              configOverrides
          )
  let compiledRecords = compileHaskellRecords schemaOverride schema
  let haskellRecordModules = generateRecordModules cfg (PD.schemaName schema) compiledRecords
  let identifiersModule = generateIdentifiersModule cfg compiledRecords
  reexportModule <-
    formatCode
      [i|module #{configModuleName} (
  module #{configModuleName}.Identifiers,
  #{T.intercalate ",\n  " . fmap (("module " <>) . fst) . L.sort $ haskellRecordModules}
) where

import #{configModuleName}.Identifiers
#{T.intercalate "\n" $ fmap (("import " <>) . fst) haskellRecordModules}
|]
  formatCode (snd identifiersModule) >>= TIO.writeFile (configDirectoryModuleIdentifiers cfg)
  TIO.writeFile (configDirectoryModule cfg <> ".hs") reexportModule
  forM_ haskellRecordModules $ \(moduleName, moduleContent) -> do
    content <- formatCode moduleContent
    TIO.writeFile (mkConfigDirectoryModuleRecord cfg (snd (T.breakOnEnd "." moduleName))) content


type RecordModule = (T.Text, T.Text)


generateIdentifiersModule :: Config -> [HaskellRecord] -> (T.Text, T.Text)
generateIdentifiersModule _cfg@MkConfig {..} records =
  let idents = L.sortOn snd $ findPrimaryKeyTypes records
      moduleName = [i|#{configModuleName}.Identifiers|]
      identTexts = T.intercalate "\n\n\n" $ fmap (\((t, _tw), n) -> mkIdentifierText n t) idents
      imports =
        T.intercalate "\n" . ("import Autodocodec.OpenAPI ()" :) . ("import Prelude (pure, (.))" :) . fmap ("import qualified " <>) . L.nub . L.sort $
          "Autodocodec"
            : "Data.Aeson"
            : "Data.Either"
            : "Data.Eq"
            : "Data.Maybe"
            : "Data.OpenApi"
            : "Data.Ord"
            : "Data.UUID"
            : "Rel8"
            : "Servant"
            : "Text.Show"
            : fmap (\((t, _tw), _n) -> T.dropEnd 1 . fst . T.breakOnEnd "." $ t) idents
   in ( moduleName
      , [i|{-\# LANGUAGE DerivingVia \#-}
{-\# LANGUAGE DuplicateRecordFields \#-}
{-\# LANGUAGE FlexibleInstances \#-}
{-\# LANGUAGE GeneralizedNewtypeDeriving \#-}
{-\# LANGUAGE NoImplicitPrelude \#-}
{-\# LANGUAGE OverloadedStrings \#-}
{-\# OPTIONS_GHC -Wno-orphans -Wno-unrecognised-pragmas \#-}
{-\# HLINT ignore "Avoid restricted alias" \#-}

-- | This module provides for identifiers definitions for records.
module #{moduleName} where

#{imports}


#{identTexts}

instance Autodocodec.HasCodec Data.UUID.UUID where
  codec =
    Autodocodec.named "UUID" codec
    where
      parse = Data.Maybe.maybe (Data.Either.Left "Invalid UUID value") pure . Data.UUID.fromText
      codec =
        Autodocodec.bimapCodec parse Data.UUID.toText Autodocodec.textCodec
          Autodocodec.<?> "Type representing Universally Unique Identifiers (UUID) as specified in RFC 4122."

        |]
      )


mkIdentifierText :: T.Text -> T.Text -> T.Text
mkIdentifierText tName tType =
  let cName = [i|Mk#{tName}|] :: T.Text
   in [i|-- | Identifier type '#{tName}'.
newtype #{tName} = #{cName}
  { _un#{tName} :: #{tType}
  }
  deriving newtype (Rel8.DBEq, Rel8.DBType, Data.Eq.Eq, Data.Ord.Ord, Text.Show.Show, Data.OpenApi.ToParamSchema, Servant.FromHttpApiData)
  deriving (Data.Aeson.FromJSON, Data.Aeson.ToJSON, Data.OpenApi.ToSchema) via (Autodocodec.Autodocodec #{tName})


instance Autodocodec.HasCodec #{tName} where
  codec =
    Autodocodec.named _type _codec Autodocodec.<?> _docs
    where
      _type = "#{tName}"
      _docs = "#{tName} Identifier."
      _codec = Autodocodec.dimapCodec #{cName} _un#{tName} Autodocodec.codec
|]


generateRecordModules :: Config -> PD.SchemaName -> [HaskellRecord] -> [RecordModule]
generateRecordModules cfg schemaName records =
  fmap (generateRecordModule cfg schemaName records) records


generateRecordModule :: Config -> PD.SchemaName -> [HaskellRecord] -> HaskellRecord -> RecordModule
generateRecordModule _cfg@MkConfig {..} schemaName _records _record@MkHaskellRecord {..} =
  let title = tableNameToDataName haskellRecordName
      qnSchemaName = PD.schemaNameToText schemaName
      qnTableName = PD.tableNameToText haskellRecordName
      tableName = [i|"#{qnTableName}"|] :: T.Text
      tableNameWithSchema = [i|"#{qnSchemaName}".#{tableName}|] :: T.Text
      recordName = [i|#{title}Record|] :: T.Text
      moduleName = [i|#{configModuleName}.Records.#{recordName}|]
      dataDefinitionKey = if length haskellRecordFields > 1 then "data" else "newtype" :: T.Text
      identifierModuleName = [i|#{configModuleName}.Identifiers|] :: T.Text
      mkFieldName = caseText CasingCamelCase . ((recordName <> "_") <>) . PD.columnNameToText
      mkOptional b t = if b then [i|(Data.Maybe.Maybe #{t})|] else t

      fieldOptis = fmap (\MkHaskellRecordField {..} -> haskellRecordFieldNullable) haskellRecordFields
      fieldTypes = fmap (\MkHaskellRecordField {..} -> resolveType configModuleName _records haskellRecordFieldType) haskellRecordFields
      fieldNames = fmap (\MkHaskellRecordField {..} -> mkFieldName haskellRecordFieldName) haskellRecordFields

      fields =
        T.intercalate "\n  , " $
          zipWith3 (\(_t, tw) f b -> [i|#{f} :: !(Rel8.Column f #{mkOptional b tw})|]) fieldTypes fieldNames fieldOptis

      autodocodecFields =
        T.intercalate "\n        Control.Applicative.<*> " $
          fmap
            ( \MkHaskellRecordField {..} ->
                [i|Autodocodec.requiredField "#{PD.columnNameToText haskellRecordFieldName}" "<undocumented>" Autodocodec..= #{mkFieldName haskellRecordFieldName}|]
            )
            haskellRecordFields

      imports =
        T.intercalate "\n" . ("import Prelude (($))" :) . fmap ("import qualified " <>) . L.nub . L.sort $
          [ "Autodocodec"
          , "Control.Applicative"
          , "Data.Aeson"
          , "Data.Eq"
          , "Data.Functor"
          , "Data.Maybe"
          , "Data.OpenApi"
          , "GHC.Generics"
          , "Rel8"
          , "Text.Show"
          ]
            <> fmap (T.dropEnd 1 . fst . T.breakOnEnd "." . fst) fieldTypes

      fieldMappings =
        T.intercalate "\n          , " $
          fmap
            ( \MkHaskellRecordField {..} ->
                [i|#{mkFieldName haskellRecordFieldName} = "#{PD.columnNameToText haskellRecordFieldName}"|]
            )
            haskellRecordFields
   in ( moduleName
      , [i|
{-\# LANGUAGE DeriveAnyClass \#-}
{-\# LANGUAGE DeriveGeneric \#-}
{-\# LANGUAGE DerivingVia \#-}
{-\# LANGUAGE DuplicateRecordFields \#-}
{-\# LANGUAGE FlexibleInstances \#-}
{-\# LANGUAGE NoImplicitPrelude \#-}
{-\# LANGUAGE OverloadedStrings \#-}
{-\# LANGUAGE StandaloneDeriving \#-}
{-\# LANGUAGE TypeOperators \#-}
{-\# OPTIONS_GHC -Wno-unrecognised-pragmas \#-}
{-\# HLINT ignore "Avoid restricted alias" \#-}

-- | This module provides for /#{title}/ record definition, its database mapping and other related definitions.
module #{moduleName} where

import #{identifierModuleName} ()
#{imports}


-- * Data Definition


-- | Data type representing the /#{title}/ record backed by database table @#{tableNameWithSchema}@.
#{dataDefinitionKey} #{recordName}F f = #{recordName}
  { #{fields}
  }
  deriving stock (GHC.Generics.Generic)
  deriving anyclass (Rel8.Rel8able)


deriving stock instance f ~ Rel8.Result => Data.Eq.Eq (#{recordName}F f)


deriving stock instance f ~ Rel8.Result => Text.Show.Show (#{recordName}F f)


-- | Result type definition for /#{title}/ record.
type #{recordName} = #{recordName}F Rel8.Result


deriving via Autodocodec.Autodocodec #{recordName} instance Data.Aeson.FromJSON #{recordName}


deriving via Autodocodec.Autodocodec #{recordName} instance Data.Aeson.ToJSON #{recordName}


deriving via Autodocodec.Autodocodec #{recordName} instance Data.OpenApi.ToSchema #{recordName}


instance Autodocodec.HasCodec #{recordName} where
  codec =
    Autodocodec.object "#{recordName}" $
      #{recordName}
        Data.Functor.<$> #{autodocodecFields}

-- * Database Mapping


-- | 'Rel8.TableSchema' definition for '#{recordName}F'.
table#{recordName} :: Rel8.TableSchema (#{recordName}F Rel8.Name)
table#{recordName} =
  Rel8.TableSchema
    { Rel8.name = Rel8.QualifiedName "#{qnTableName}" (Data.Maybe.Just "#{qnSchemaName}")
    , Rel8.columns =
        #{recordName}
          { #{fieldMappings}
          }
    }
|]
      )


type ResolvedType = (T.Text, T.Text)


resolveType :: T.Text -> [HaskellRecord] -> HaskellRecordFieldType -> ResolvedType
resolveType mn rs ft =
  case ft of
    MkHaskellRecordFieldTypePgType (_, pt) -> pgTypeToHaskellType pt
    MkHaskellRecordFieldTypePrimaryKey (_, _, PD.MkColumnReference {..}) ->
      let tableName = PD.tableNameToText columnReferenceTable
          columnName = PD.columnNameToText columnReferenceColumn
          dataName = caseText CasingPascalCase [i|#{tableName}_#{columnName}|]
          fullName = [i|#{mn}.Identifiers.#{dataName}|]
       in (fullName, fullName)
    MkHaskellRecordFieldTypeForeignKey (_, PD.MkColumnReference {..}) ->
      let xx =
            head
              [ haskellRecordFieldType
              | MkHaskellRecord {..} <- rs
              , haskellRecordName == columnReferenceTable
              , MkHaskellRecordField {..} <- haskellRecordFields
              , haskellRecordFieldName == columnReferenceColumn
              ]
       in resolveType mn rs xx
    MkHaskellRecordFieldTypeCustom (_, t) -> (t, t)


findPrimaryKeyTypes :: [HaskellRecord] -> [((T.Text, T.Text), T.Text)]
findPrimaryKeyTypes records =
  findPrimaryKeyTypes' $
    [ haskellRecordFieldType
    | MkHaskellRecord {..} <- records
    , MkHaskellRecordField {..} <- haskellRecordFields
    ]


findPrimaryKeyTypes' :: [HaskellRecordFieldType] -> [((T.Text, T.Text), T.Text)]
findPrimaryKeyTypes' =
  mapMaybe findPrimaryKeyType


findPrimaryKeyType :: HaskellRecordFieldType -> Maybe ((T.Text, T.Text), T.Text)
findPrimaryKeyType ft =
  case ft of
    MkHaskellRecordFieldTypePrimaryKey (_, pt, PD.MkColumnReference {..}) ->
      let tableName = PD.tableNameToText columnReferenceTable
          columnName = PD.columnNameToText columnReferenceColumn
          dataName = caseText CasingPascalCase [i|#{tableName}_#{columnName}|]
       in Just (pgTypeToHaskellType pt, dataName)
    _ -> Nothing


pgTypeToHaskellType :: PD.PgType -> ResolvedType
pgTypeToHaskellType pt =
  case pt of
    PD.PgTypeBit -> ("Data.Text.Text", "Data.Text.Text") -- TODO: Check this type mapping
    PD.PgTypeBool -> ("Data.Bool.Bool", "Data.Bool.Bool")
    PD.PgTypeBpchar -> ("Data.Char.Char", "Data.Char.Char")
    PD.PgTypeBytea -> ("Data.ByteString.ByteString", "Data.ByteString.ByteString")
    PD.PgTypeCidr -> ("Data.Text.Text", "Data.Text.Text") -- TODO: Check this type mapping
    PD.PgTypeDate -> ("Data.Time.Day", "Data.Time.Day")
    PD.PgTypeFloat4 -> ("GHC.Float.Float", "GHC.Float.Float")
    PD.PgTypeFloat8 -> ("GHC.Float.Double", "GHC.Float.Double")
    PD.PgTypeInet -> ("Data.Text.Text", "Data.Text.Text") -- TODO: Check this type mapping
    PD.PgTypeInt2 -> ("Data.Int.Int16", "Data.Int.Int16")
    PD.PgTypeInt4 -> ("Data.Int.Int32", "Data.Int.Int32")
    PD.PgTypeInt8 -> ("Data.Int.Int64", "Data.Int.Int64")
    PD.PgTypeJson -> ("Data.Aeson.Value", "Data.Aeson.Value")
    PD.PgTypeJsonb -> ("Data.Aeson.Value", "Data.Aeson.Value")
    PD.PgTypeMacaddr -> ("Data.Text.Text", "Data.Text.Text") -- TODO: Check this type mapping
    PD.PgTypeMacaddr8 -> ("Data.Text.Text", "Data.Text.Text") -- TODO: Check this type mapping
    PD.PgTypeNumeric -> ("Data.Scientific.Scientific", "Data.Scientific.Scientific")
    PD.PgTypeText -> ("Data.Text.Text", "Data.Text.Text")
    PD.PgTypeTime -> ("Data.Time.TimeOfDay", "Data.Time.TimeOfDay")
    PD.PgTypeTimestamp -> ("Data.Time.LocalTime", "Data.Time.LocalTime")
    PD.PgTypeTimestamptz -> ("Data.Time.UTCTime", "Data.Time.UTCTime")
    PD.PgTypeUuid -> ("Data.UUID.UUID", "Data.UUID.UUID")
    PD.PgTypeVarchar -> ("Data.Text.Text", "Data.Text.Text")
    PD.PgTypeArray pgt -> (fst (pgTypeToHaskellType pgt), [i|[#{fst (pgTypeToHaskellType pgt)}]|])


formatCode :: T.Text -> IO T.Text
formatCode src = do
  let src' = TP.byteStringInput (TLE.encodeUtf8 (TL.fromStrict src))
  let proc = TP.setStdin src' $ TP.proc "fourmolu" ["--stdin-input-file", "-"]
  (exitCode, out, err) <- TP.readProcess proc
  case exitCode of
    ExitSuccess -> pure (TL.toStrict (TLE.decodeUtf8 out))
    ExitFailure _ -> error ("ERROR: Failed to format Haskell code using fourmolu" <> BLC.unpack err <> ". Code to be formatted:\n" <> T.unpack src)
