{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Postmap.Introspect where

import qualified Autodocodec as ADC
import qualified Data.Aeson as Aeson
import Data.Foldable (foldl')
import Data.Functor.Identity (Identity)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified Hasql.Connection
import qualified Hasql.Session
import qualified Hasql.Statement
import qualified Hasql.TH
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import qualified Text.Parsec
import qualified Text.Parsec.Language
import qualified Text.Parsec.Token


-- | Data definition for a database table.
data Table = Table
  { tableSchema :: !TableSchemaName
  , tableName :: !TableName
  , tableColumns :: ![Column]
  , tableUniques :: ![[ColumnName]]
  , tablePrimaryKey :: !(Maybe ColumnName)
  }
  deriving stock (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Table)


instance ADC.HasCodec Table where
  codec =
    ADC.object "Table" $
      Table
        <$> ADC.requiredField "schema" "Schema the table belongs to." ADC..= tableSchema
        <*> ADC.requiredField "name" "Name of the table." ADC..= tableName
        <*> ADC.requiredField "columns" "Columns of the table." ADC..= tableColumns
        <*> ADC.requiredField "uniques" "List of list of column names forming unique constraints." ADC..= tableUniques
        <*> ADC.requiredField "primary_key" "Name of the primary key column, if any." ADC..= tablePrimaryKey


newtype TableSchemaName = MkTableSchemaName
  { unTableSchemaName :: T.Text
  }
  deriving stock (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec TableSchemaName)


instance Hashable TableSchemaName


instance ADC.HasCodec TableSchemaName where
  codec =
    ADC.bimapCodec mkTableSchemaName unTableSchemaName ADC.textCodec


mkTableSchemaName :: T.Text -> Either String TableSchemaName
mkTableSchemaName s =
  either (Left . show) (Right . MkTableSchemaName . T.pack) (Text.Parsec.parse parser "" (T.unpack s))
  where
    parser = Text.Parsec.Token.identifier tokenParser


newtype TableName = MkTableName
  { unTableName :: T.Text
  }
  deriving stock (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec TableName)


instance Hashable TableName


instance ADC.HasCodec TableName where
  codec =
    ADC.bimapCodec mkTableName unTableName ADC.textCodec


mkTableName :: T.Text -> Either String TableName
mkTableName s =
  either (Left . show) (Right . MkTableName . T.pack) (Text.Parsec.parse parser "" (T.unpack s))
  where
    parser = Text.Parsec.Token.identifier tokenParser


-- | Data definition for a database column.
data Column = Column
  { columnName :: !ColumnName
  , columnType :: !T.Text -- TODO: Use a proper type.
  , columnNullable :: !Bool
  , columnIsPrimaryKey :: !Bool
  , columnForeignKey :: !(Maybe ColumnReference)
  , columnUnique :: !Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Column)


instance ADC.HasCodec Column where
  codec =
    ADC.object "Column" $
      Column
        <$> ADC.requiredField "name" "Name of the column." ADC..= columnName
        <*> ADC.requiredField "type" "Type of the column." ADC..= columnType
        <*> ADC.requiredField "nullable" "Whether the column is nullable." ADC..= columnNullable
        <*> ADC.requiredField "is_primary_key" "Whether the column is a primary key." ADC..= columnIsPrimaryKey
        <*> ADC.requiredField "foreign_key" "Foreign key reference, if any." ADC..= columnForeignKey
        <*> ADC.requiredField "unique" "Whether the column is unique (alone)." ADC..= columnUnique


newtype ColumnName = MkColumnName
  { unColumnName :: T.Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ColumnName)


instance Hashable ColumnName


instance ADC.HasCodec ColumnName where
  codec =
    ADC.bimapCodec mkColumnName unColumnName ADC.textCodec


mkColumnName :: T.Text -> Either String ColumnName
mkColumnName s =
  either (Left . show) (Right . MkColumnName . T.pack) (Text.Parsec.parse parser "" (T.unpack s))
  where
    parser = Text.Parsec.Token.identifier tokenParser


-- | Data definition for a foreign key reference.
data ColumnReference = ColumnReference
  { columnReferenceSchema :: !TableSchemaName
  , columnReferenceTable :: !TableName
  , columnReferenceColumn :: !ColumnName
  }
  deriving stock (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ColumnReference)


instance ADC.HasCodec ColumnReference where
  codec =
    ADC.object "ColumnReference" $
      ColumnReference
        <$> ADC.requiredField "schema" "Schema of the foreign table." ADC..= columnReferenceSchema
        <*> ADC.requiredField "table" "Name of the foreign table." ADC..= columnReferenceTable
        <*> ADC.requiredField "column" "Name of the foreign column." ADC..= columnReferenceColumn


-- * Internal


-- ** Fetching Schema


-- | Connects to the database and fetches the database schema for the
-- given schema name.
fetchSchema :: Hasql.Connection.Connection -> T.Text -> IO [Table]
fetchSchema conn schema = do
  columns <- fetchColumns conn schema
  primaryKeys <- fetchPrimaryKeys conn schema
  foreignKeys <- fetchForeignKeys conn schema
  uniqueConstraints <- fetchUniqueConstraints conn schema
  let dbPks = compilePrimaryKeys primaryKeys
  let dbFks = compileForeignKeys foreignKeys
  let dbUcs = compileUniqueConstraints uniqueConstraints
  let dbRef = (dbPks, dbFks, dbUcs) :: References
  pure . List.sortOn ((<>) <$> unTableSchemaName . tableSchema <*> unTableName . tableName) . fmap snd . HM.toList $ foldl' (go dbRef) mempty columns
  where
    go refs db c@(schemaName, tableName, _, _, _) =
      addColumn refs c (addTable refs schemaName tableName db)


type DbTables = HM.HashMap (TableSchemaName, TableName) Table


type DbPrimaryKeys = HM.HashMap (TableSchemaName, TableName) ColumnName


type DbForeignKeys = HM.HashMap (TableSchemaName, TableName, ColumnName) (TableSchemaName, TableName, ColumnName)


type DbUniqueConstraints = HM.HashMap (TableSchemaName, TableName) [[ColumnName]]


type References = (DbPrimaryKeys, DbForeignKeys, DbUniqueConstraints)


addTable :: References -> TableSchemaName -> TableName -> DbTables -> DbTables
addTable (pks, _fks, ucs) tableSchema tableName db =
  maybe (HM.insert key newTable db) (const db) (HM.lookup key db)
  where
    key = (tableSchema, tableName)
    newTable =
      Table
        { tableSchema
        , tableName
        , tableColumns = []
        , tableUniques = fromMaybe [] (HM.lookup key ucs)
        , tablePrimaryKey = HM.lookup key pks
        }


addColumn :: References -> StmtTypeColumns -> DbTables -> DbTables
addColumn (pks, fks, ucs) (schemaName, tableName, columnName, columnNullable, columnType) db =
  maybe db (\t -> HM.insert key (t {tableColumns = tableColumns t <> [newColumn]}) db) (HM.lookup key db)
  where
    key = (schemaName, tableName)
    newColumn =
      Column
        { columnName
        , columnType
        , columnNullable
        , columnIsPrimaryKey = Just columnName == HM.lookup key pks
        , columnForeignKey =
            case HM.lookup (schemaName, tableName, columnName) fks of
              Nothing -> Nothing
              Just (columnReferenceSchema, columnReferenceTable, columnReferenceColumn) -> Just ColumnReference {columnReferenceSchema, columnReferenceTable, columnReferenceColumn}
        , columnUnique = [columnName] `elem` fromMaybe [] (HM.lookup key ucs)
        }


compilePrimaryKeys :: [StmtTypePrimaryKeys] -> DbPrimaryKeys
compilePrimaryKeys =
  HM.fromList . fmap (\(schema, table, column) -> ((schema, table), column))


compileForeignKeys :: [StmtTypeForeignKeys] -> DbForeignKeys
compileForeignKeys =
  HM.fromList . fmap (\(schema, table, column, fSchema, fTable, fColumn) -> ((schema, table, column), (fSchema, fTable, fColumn)))


compileUniqueConstraints :: [StmtTypeUniqueConstraints] -> DbUniqueConstraints
compileUniqueConstraints =
  foldl' go mempty
  where
    go acc (schema, table, _constraint, columns) =
      case HM.lookup (schema, table) acc of
        Nothing -> HM.insert (schema, table) [V.toList columns] acc
        Just xs -> HM.insert (schema, table) (V.toList columns : xs) acc


-- ** Fetching Columns


type StmtTypeColumns =
  ( TableSchemaName -- Table schema.
  , TableName -- Table name.
  , ColumnName -- Column name.
  , Bool -- Column is nullable.
  , T.Text -- Column type.
  )


type StmtTypeColumns' =
  ( T.Text -- Table schema.
  , T.Text -- Table name.
  , T.Text -- Column name.
  , Bool -- Column is nullable.
  , T.Text -- Column type.
  )


toStmtTypeColumns :: StmtTypeColumns' -> StmtTypeColumns
toStmtTypeColumns (schema, table, column, nullable, typ) =
  (MkTableSchemaName schema, MkTableName table, MkColumnName column, nullable, typ)


fetchColumns :: Hasql.Connection.Connection -> T.Text -> IO [StmtTypeColumns]
fetchColumns conn schema = do
  result <- Hasql.Session.run (Hasql.Session.statement schema stmtColumns) conn
  either (_die . show) (pure . fmap toStmtTypeColumns . V.toList) result


stmtColumns :: Hasql.Statement.Statement T.Text (V.Vector StmtTypeColumns')
stmtColumns =
  [Hasql.TH.vectorStatement|
    SELECT "table_schema" :: text
         , "table_name" :: text
         , "column_name" :: text
         , "is_nullable" :: bool
         , "udt_name" :: text
      FROM "information_schema"."columns"
      WHERE "table_schema" = $1 :: text
      ORDER BY "table_name", "ordinal_position"
  |]


-- ** Fetching Primary Keys


type StmtTypePrimaryKeys =
  ( TableSchemaName -- Table schema.
  , TableName -- Table name.
  , ColumnName -- Column name.
  )


type StmtTypePrimaryKeys' =
  ( T.Text -- Table schema.
  , T.Text -- Table name.
  , T.Text -- Column name.
  )


toStmtTypePrimaryKeys :: StmtTypePrimaryKeys' -> StmtTypePrimaryKeys
toStmtTypePrimaryKeys (schema, table, column) =
  (MkTableSchemaName schema, MkTableName table, MkColumnName column)


fetchPrimaryKeys :: Hasql.Connection.Connection -> T.Text -> IO [StmtTypePrimaryKeys]
fetchPrimaryKeys conn schema = do
  result <- Hasql.Session.run (Hasql.Session.statement schema stmtPrimaryKeys) conn
  either (_die . show) (pure . fmap toStmtTypePrimaryKeys . V.toList) result


stmtPrimaryKeys :: Hasql.Statement.Statement T.Text (V.Vector StmtTypePrimaryKeys')
stmtPrimaryKeys =
  [Hasql.TH.vectorStatement|
    SELECT $1 :: text as "table_schema"
         , "pg_class"."oid"::regclass :: text as "table_name"
         , "pg_attribute"."attname" :: text as "column_name"
      FROM "pg_index"
         , "pg_class"
         , "pg_attribute"
         , "pg_namespace"
     WHERE "pg_namespace"."nspname" = $1 :: text
       AND "pg_namespace"."oid" = "pg_class"."relnamespace"
       AND "pg_index"."indrelid" = "pg_class"."oid"
       AND "pg_index"."indisprimary"
       AND "pg_attribute"."attrelid" = "pg_class"."oid"
       AND "pg_attribute"."attnum" = any("pg_index"."indkey")
  |]


-- ** Fetching Foreign Keys


type StmtTypeForeignKeys =
  ( TableSchemaName -- Table schema.
  , TableName -- Table name.
  , ColumnName -- Column name.
  , TableSchemaName -- Foreign table schema.
  , TableName -- Foreign table name.
  , ColumnName -- Foreign column name.
  )


type StmtTypeForeignKeys' =
  ( T.Text -- Table schema.
  , T.Text -- Table name.
  , T.Text -- Column name.
  , T.Text -- Foreign table schema.
  , T.Text -- Foreign table name.
  , T.Text -- Foreign column name.
  )


toStmtTypeForeignKeys :: StmtTypeForeignKeys' -> StmtTypeForeignKeys
toStmtTypeForeignKeys (schema, table, column, fSchema, fTable, fColumn) =
  (MkTableSchemaName schema, MkTableName table, MkColumnName column, MkTableSchemaName fSchema, MkTableName fTable, MkColumnName fColumn)


fetchForeignKeys :: Hasql.Connection.Connection -> T.Text -> IO [StmtTypeForeignKeys]
fetchForeignKeys conn schema = do
  result <- Hasql.Session.run (Hasql.Session.statement schema stmtForeignKeys) conn
  either (_die . show) (pure . fmap toStmtTypeForeignKeys . V.toList) result


stmtForeignKeys :: Hasql.Statement.Statement T.Text (V.Vector StmtTypeForeignKeys')
stmtForeignKeys =
  [Hasql.TH.vectorStatement|
    SELECT "tc"."table_schema" :: text as "table_schema"
         , "tc"."table_name" :: text as "table_name"
         , "kcu"."column_name" :: text as "column_name"
         , "ccu"."table_schema" :: text as "foreign_table_schema"
         , "ccu"."table_name" :: text as "foreign_table_name"
         , "ccu"."column_name" :: text as "foreign_column_name"
      FROM "information_schema"."table_constraints" AS "tc"
         , "information_schema"."key_column_usage" AS "kcu"
         , "information_schema"."constraint_column_usage" AS "ccu"
     WHERE "tc"."table_schema" = $1 :: text
       AND "tc"."constraint_type" = 'FOREIGN KEY'
       AND "tc"."constraint_name" = "kcu"."constraint_name"
       AND "tc"."table_schema" = "kcu"."table_schema"
       AND "ccu"."constraint_name" = "tc"."constraint_name"
  |]


-- ** Fetching Unique Constraints


type StmtTypeUniqueConstraints =
  ( TableSchemaName -- Table schema.
  , TableName -- Table name.
  , T.Text -- Constraint name.
  , V.Vector ColumnName -- Column names.
  )


type StmtTypeUniqueConstraints' =
  ( T.Text -- Table schema.
  , T.Text -- Table name.
  , T.Text -- Constraint name.
  , V.Vector T.Text -- Column names.
  )


toStmtTypeUniqueConstraints :: StmtTypeUniqueConstraints' -> StmtTypeUniqueConstraints
toStmtTypeUniqueConstraints (schema, table, constraint, columns) =
  (MkTableSchemaName schema, MkTableName table, constraint, V.map MkColumnName columns)


fetchUniqueConstraints :: Hasql.Connection.Connection -> T.Text -> IO [StmtTypeUniqueConstraints]
fetchUniqueConstraints conn schema = do
  result <- Hasql.Session.run (Hasql.Session.statement schema stmtUniqueConstraints) conn
  either (_die . show) (pure . fmap toStmtTypeUniqueConstraints . V.toList) result


stmtUniqueConstraints :: Hasql.Statement.Statement T.Text (V.Vector StmtTypeUniqueConstraints')
stmtUniqueConstraints =
  [Hasql.TH.vectorStatement|
    SELECT $1 :: text as "table_schema"
         , "c"."conrelid"::regclass :: text AS "table_name"
         , "c"."conname" :: text AS "constraint_name"
         , array_agg("a"."attname" :: text ORDER BY "k"."n") :: text[] AS "columns"
      FROM "pg_constraint" AS "c"
         CROSS JOIN LATERAL unnest("c"."conkey") WITH ORDINALITY AS k("c", "n")
         JOIN "pg_attribute" AS "a" ON "a"."attnum" = "k"."c" AND "a"."attrelid" = "c"."conrelid"
     WHERE "c"."contype" = 'u'
       AND "c"."connamespace" = $1 :: text :: regnamespace
     GROUP BY "c"."oid", "c"."conrelid", "c"."conname"
  |]


-- ** Helpers


tokenParser :: Text.Parsec.Token.GenTokenParser String u Identity
tokenParser =
  Text.Parsec.Token.makeTokenParser Text.Parsec.Language.haskellStyle


-- | Prints the given error messages and kills the program.
--
-- __TODO:__ Drop this in favour of propogating errors using MonadError or MonadThrow.
_die :: String -> IO a
_die msg = do
  hPutStrLn stderr [i|"ERROR: #{msg}"|]
  die "Exiting..."
