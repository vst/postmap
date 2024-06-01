{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Postmap.Gencode.Haskell where

import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.List as List
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.String.Interpolate (i)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Postmap.Introspect (ColumnName (..), TableName (..), TableSchemaName (..))
import Postmap.Spec (Field (..), FieldName (..), FieldReference (..), Record (..), RecordName (..), Spec (..))
import System.Exit
import qualified System.Process.Typed as TP
import qualified Text.Casing as Casing
import qualified Text.Pandoc as Pandoc


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
  contentIdsModule <- formatCode $ mkIdsModule config specRecords
  reexportModule <-
    formatCode
      [i|module #{configModuleName} (
  module #{configModuleName}.Identifiers,
  #{T.intercalate ",\n  " $ fmap ("module " <>) recmods}
) where

import #{configModuleName}.Identifiers
#{T.intercalate "\n" $ fmap ("import " <>) recmods}
|]
  TIO.writeFile (configOutputIdsModuleFile config) contentIdsModule
  TIO.writeFile (configOutputDirectory config <> ".hs") reexportModule
  putStrLn "Haskell code generation completed."
  putStrLn "Some extensions are used. You may want to use following hlint rule:\n"
  let extensions = ["DeriveAnyClass", "DeriveGeneric", "DerivingVia", "DuplicateRecordFields", "FlexibleInstances", "NoImplicitPrelude", "OverloadedStrings", "StandaloneDeriving", "TypeOperators"]
  putStrLn "- extensions:"
  putStrLn "    - default: false # All extension are banned by default."
  putStrLn "    - name:"
  mapM_ (putStrLn . ("        - " <>)) extensions
  putStrLn "\nYou may also want to use following hlint rule for qualified imports:\n"
  putStrLn [i|    - { name: #{configModuleName}, as: DB.Model, importStyle: qualified, asRequired: true }|]
  putStrLn [i|    - { name: #{configModuleName}.**, within: [#{configModuleName}, #{configModuleName}.**] }|]


mkIdsModule :: Config -> [Record] -> T.Text
mkIdsModule Config {..} records =
  let ids = mapMaybe mkRecordId (List.nub records)
      modules' = mapMaybe fst ids
      modules =
        List.nub . List.sort $
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
            : modules'
   in [i|{-\# LANGUAGE DerivingVia \#-}
{-\# LANGUAGE DuplicateRecordFields \#-}
{-\# LANGUAGE FlexibleInstances \#-}
{-\# LANGUAGE GeneralizedNewtypeDeriving \#-}
{-\# LANGUAGE NoImplicitPrelude \#-}
{-\# LANGUAGE OverloadedStrings \#-}
{-\# OPTIONS_GHC -Wno-orphans -Wno-unrecognised-pragmas \#-}
{-\# HLINT ignore "Avoid restricted alias" \#-}

-- | This module provides for identifiers definitions for records.
module #{configModuleName}.Identifiers where

import Autodocodec.OpenAPI ()
import Prelude (pure, (.))
#{T.intercalate "\n" (fmap ("import qualified " <>) modules)}


#{T.intercalate "\n\n" (fmap snd ids)}


instance Autodocodec.HasCodec Data.UUID.UUID where
  codec =
    Autodocodec.named "UUID" codec
    where
      parse = Data.Maybe.maybe (Data.Either.Left "Invalid UUID value") pure . Data.UUID.fromText
      codec =
        Autodocodec.bimapCodec parse Data.UUID.toText Autodocodec.textCodec
          Autodocodec.<?> "Type representing Universally Unique Identifiers (UUID) as specified in RFC 4122."
|]


mkRecordId :: Record -> Maybe (Maybe T.Text, T.Text)
mkRecordId Record {..} =
  case List.find (\x -> fieldIsPrimaryKey x && isNothing (fieldReference x)) recordFields of
    Nothing -> Nothing
    Just Field {..} ->
      Just $
        let tName = unRecordName recordName <> "Id"
            cName = "Mk" <> tName
            title = fromMaybe (unRecordName recordName) recordTitle
            tType = fromMaybe (snd $ defFieldType fieldColumnType) fieldType
            tModule = filterMaybe (not . T.null) . T.dropEnd 1 . T.dropWhileEnd (/= '.') $ tType
         in ( tModule
            , [i|-- | Identifier type for "#{title}" record.
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
      _docs = "#{title} Identifier."
      _codec = Autodocodec.dimapCodec #{cName} _un#{tName} Autodocodec.codec
|]
            )


generateRecord :: Config -> Record -> IO T.Text
generateRecord config@Config {..} record = do
  (n, dt) <- mkRecordDataType config record
  content <- formatCode dt
  TIO.writeFile (configOutputRecordModuleFile config n) content
  pure $ configModuleName <> ".Records." <> n


mkRecordDataType :: Config -> Record -> IO (T.Text, T.Text)
mkRecordDataType config@Config {..} record@Record {..} = do
  docs <- maybe (pure Nothing) (fmap (Just . T.unlines . fmap ("-- " <>) . T.lines) . mdToHaddock) recordDescription
  iFlds <- mapM (mkRecordDataTypeField config record) recordFields
  let hkdName = mkRecordHkdTypeName record
      cnsName = mkRecordConstructorName record
      title = fromMaybe (unRecordName recordName) recordTitle
      table = [i|"#{unTableSchemaName recordTableSchema}"."#{unTableName recordTableName}"|] :: T.Text
      fields = T.intercalate "\n  , " $ fmap snd iFlds
      hasMultipleFields = length iFlds > 1
      modules'' = mapMaybe (filterMaybe (not . T.null) . T.dropEnd 1 . T.dropWhileEnd (/= '.') . fst) iFlds
      modules' = ["Data.Maybe" | not (all fieldNotNullable recordFields)] <> modules''
      modules =
        filter (if length iFlds > 1 then const True else (/= "Control.Applicative")) . List.nub . List.sort $
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
{-\# LANGUAGE NoImplicitPrelude \#-}
{-\# LANGUAGE OverloadedStrings \#-}
{-\# LANGUAGE StandaloneDeriving \#-}
{-\# LANGUAGE TypeOperators \#-}
{-\# OPTIONS_GHC -Wno-unrecognised-pragmas \#-}
{-\# HLINT ignore "Avoid restricted alias" \#-}

-- | This module provides for /#{title}/ record definition, its database mapping and other related definitions.
module #{configModuleName}.Records.#{cnsName} where

import #{configModuleName}.Identifiers
import Prelude (($))
#{T.intercalate "\n" (fmap ("import qualified " <>) modules)}


-- * Data Definition


-- | Data type representing the /#{title}/ record backed by database table @#{table}@.#{maybe "" (\x -> "\n--\n" <> x <> "\n") docs}
#{if hasMultipleFields then "data" else "newtype" :: T.Text} #{hkdName} f = #{cnsName}
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
   in pure (cnsName, content)


mkRecordDataTypeField :: Config -> Record -> Field -> IO (T.Text, T.Text)
mkRecordDataTypeField _config record@Record {..} field@Field {..} = do
  docs <- maybe (pure Nothing) (fmap (Just . T.unlines . fmap ("-- " <>) . T.lines . ("^" <>)) . mdToHaddock) fieldDescription
  let fName = mkRecordFieldName record field
      (isArr, fType') = case fieldReference of
        Just FieldReference {..} -> (False, mkRecordIdTypeName fieldReferenceRecord)
        Nothing ->
          if fieldIsPrimaryKey
            then (False, mkRecordIdTypeName recordName)
            else maybe (defFieldType fieldColumnType) (False,) fieldType
      fType'' = if fieldNotNullable then fType' else [i|(Data.Maybe.Maybe #{fType'})|]
      fType = if isArr then [i|[#{fType''}]|] else fType''
      fDesc = fromMaybe "" docs
      ffType =
        if length recordFields > 1
          then [i|!(Rel8.Column f #{fType})|]
          else [i|Rel8.Column f #{fType}|] :: T.Text
   in pure (fType', [i|#{fName} :: #{ffType}#{fDesc}|])


mkRecordJsonField :: Record -> Field -> T.Text
mkRecordJsonField record field@Field {..} =
  let jName = T.pack . Casing.snake . T.unpack $ unFieldName fieldName
      fName = mkRecordFieldName record field
      fDesc = show $ fromMaybe "<undocumented>" fieldDescription
   in [i|Autodocodec.requiredField "#{jName}" #{fDesc} Autodocodec..= #{fName}|]


mkRecordColMapping :: Record -> Field -> T.Text
mkRecordColMapping record field@Field {..} =
  let fName = mkRecordFieldName record field
      fCol = unColumnName fieldColumnName
   in [i|#{fName} = "#{fCol}"|]


defFieldType :: T.Text -> (Bool, T.Text)
defFieldType "date" = (False, "Data.Time.Day")
defFieldType "time" = (False, "Data.Time.TimeOfDay")
defFieldType "timestamp" = (False, "Data.Time.LocalTime")
defFieldType "timestamptz" = (False, "Data.Time.UTCTime")
defFieldType "jsonb" = (False, "Data.Aeson.Value")
defFieldType "json" = (False, "Data.Aeson.Value")
defFieldType "uuid" = (False, "Data.UUID.UUID")
defFieldType "text" = (False, "Data.Text.Text")
defFieldType "varchar" = (False, "Data.Text.Text")
defFieldType "int2" = (False, "Data.Int.Int16")
defFieldType "int4" = (False, "Data.Int.Int32")
defFieldType "int8" = (False, "Data.Int.Int64")
defFieldType "float4" = (False, "GHC.Float.Float")
defFieldType "float8" = (False, "GHC.Float.Double")
defFieldType "numeric" = (False, "Data.Scientific.Scientific")
defFieldType "bool" = (False, "Data.Bool.Bool")
defFieldType "bytea" = (False, "Data.ByteString.ByteString")
defFieldType "inet" = (False, "Data.Text.Text")
defFieldType "cidr" = (False, "Data.Text.Text")
defFieldType "macaddr" = (False, "Data.Text.Text")
defFieldType "macaddr8" = (False, "Data.Text.Text")
defFieldType "bit" = (False, "Data.Text.Text")
defFieldType x = case T.unpack x of
  '_' : st -> (True, snd $ defFieldType (T.pack st))
  _ -> (False, [i|<unknown database column type to map: #{x}>|])


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
  unRecordName rn <> "Id"


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


-- | Runs external program "fourmolu" to format Haskell code.
formatCode :: T.Text -> IO T.Text
formatCode src = do
  let src' = TP.byteStringInput (TLE.encodeUtf8 (TL.fromStrict src))
  let proc = TP.setStdin src' $ TP.proc "fourmolu" ["--stdin-input-file", "-"]
  (exitCode, out, err) <- TP.readProcess proc
  case exitCode of
    ExitSuccess -> pure (TL.toStrict (TLE.decodeUtf8 out))
    ExitFailure _ -> error ("ERROR: Failed to format Haskell code using fourmolu" <> BLC.unpack err <> ". Code to be formatted:\n" <> T.unpack src)


mdToHaddock :: T.Text -> IO T.Text
mdToHaddock txt = Pandoc.runIOorExplode $ do
  md <- Pandoc.readMarkdown Pandoc.def {Pandoc.readerExtensions = readerExtensions} txt
  Pandoc.writeHaddock Pandoc.def {Pandoc.writerReferenceLinks = True} md
  where
    readerExtensions = Pandoc.enableExtension Pandoc.Ext_definition_lists Pandoc.githubMarkdownExtensions
