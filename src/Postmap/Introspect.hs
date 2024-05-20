{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Postmap.Introspect where

import qualified Autodocodec as ADC
import qualified Data.Aeson as Aeson
import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as HM
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


-- | Data definition for a database table.
data Table = Table
  { tableSchema :: !T.Text
  , tableName :: !T.Text
  , tableColumns :: ![Column]
  , tableUniques :: ![[T.Text]]
  , tablePrimaryKey :: !(Maybe T.Text)
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


-- | Data definition for a database column.
data Column = Column
  { columnName :: !T.Text
  , columnType :: !T.Text
  , columnNullable :: !Bool
  , columnIsPrimaryKey :: !Bool
  , columnForeignKey :: !(Maybe Reference)
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


-- | Data definition for a foreign key reference.
data Reference = Reference
  { relationSchema :: !T.Text
  , relationTable :: !T.Text
  , relationColumn :: !T.Text
  }
  deriving stock (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Reference)


instance ADC.HasCodec Reference where
  codec =
    ADC.object "Reference" $
      Reference
        <$> ADC.requiredField "schema" "Schema of the foreign table." ADC..= relationSchema
        <*> ADC.requiredField "table" "Name of the foreign table." ADC..= relationTable
        <*> ADC.requiredField "column" "Name of the foreign column." ADC..= relationColumn


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
  pure . List.sortOn ((<>) <$> tableSchema <*> tableName) . fmap snd . HM.toList $ foldl' (go dbRef) mempty columns
  where
    go refs db c@(schemaName, tableName, _, _, _) =
      addColumn refs c (addTable refs schemaName tableName db)


type DbTables = HM.HashMap (T.Text, T.Text) Table


type DbPrimaryKeys = HM.HashMap (T.Text, T.Text) T.Text


type DbForeignKeys = HM.HashMap (T.Text, T.Text, T.Text) (T.Text, T.Text, T.Text)


type DbUniqueConstraints = HM.HashMap (T.Text, T.Text) [[T.Text]]


type References = (DbPrimaryKeys, DbForeignKeys, DbUniqueConstraints)


addTable :: References -> T.Text -> T.Text -> DbTables -> DbTables
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
              Just (relationSchema, relationTable, relationColumn) -> Just Reference {relationSchema, relationTable, relationColumn}
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
  ( T.Text -- Table schema.
  , T.Text -- Table name.
  , T.Text -- Column name.
  , Bool -- Column is nullable.
  , T.Text -- Column type.
  )


fetchColumns :: Hasql.Connection.Connection -> T.Text -> IO [StmtTypeColumns]
fetchColumns conn schema = do
  result <- Hasql.Session.run (Hasql.Session.statement schema stmtColumns) conn
  either (_die . show) (pure . V.toList) result


stmtColumns :: Hasql.Statement.Statement T.Text (V.Vector StmtTypeColumns)
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
  ( T.Text -- Table schema.
  , T.Text -- Table name.
  , T.Text -- Column name.
  )


fetchPrimaryKeys :: Hasql.Connection.Connection -> T.Text -> IO [StmtTypePrimaryKeys]
fetchPrimaryKeys conn schema = do
  result <- Hasql.Session.run (Hasql.Session.statement schema stmtPrimaryKeys) conn
  either (_die . show) (pure . V.toList) result


stmtPrimaryKeys :: Hasql.Statement.Statement T.Text (V.Vector StmtTypePrimaryKeys)
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
  ( T.Text -- Table schema.
  , T.Text -- Table name.
  , T.Text -- Column name.
  , T.Text -- Foreign table schema.
  , T.Text -- Foreign table name.
  , T.Text -- Foreign column name.
  )


fetchForeignKeys :: Hasql.Connection.Connection -> T.Text -> IO [StmtTypeForeignKeys]
fetchForeignKeys conn schema = do
  result <- Hasql.Session.run (Hasql.Session.statement schema stmtForeignKeys) conn
  either (_die . show) (pure . V.toList) result


stmtForeignKeys :: Hasql.Statement.Statement T.Text (V.Vector StmtTypeForeignKeys)
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
  ( T.Text -- Table schema.
  , T.Text -- Table name.
  , T.Text -- Constraint name.
  , V.Vector T.Text -- Column names.
  )


fetchUniqueConstraints :: Hasql.Connection.Connection -> T.Text -> IO [StmtTypeUniqueConstraints]
fetchUniqueConstraints conn schema = do
  result <- Hasql.Session.run (Hasql.Session.statement schema stmtUniqueConstraints) conn
  either (_die . show) (pure . V.toList) result


stmtUniqueConstraints :: Hasql.Statement.Statement T.Text (V.Vector StmtTypeUniqueConstraints)
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


-- | Prints the given error messages and kills the program.
--
-- __TODO:__ Drop this in favour of propogating errors using MonadError or MonadThrow.
_die :: String -> IO a
_die msg = do
  hPutStrLn stderr [i|"ERROR: #{msg}"|]
  die "Exiting..."
