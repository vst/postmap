{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | __WARNING:__ This module assumes that all our names are valid
-- Postmap identifiers.
module Postmap.Database.Introspect where

import Data.List.NonEmpty (groupWith)
import qualified Data.List.NonEmpty as NE
import Data.Profunctor (dimap)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Hasql.Connection
import qualified Hasql.Session
import qualified Hasql.Statement
import qualified Hasql.TH
import Postmap.Database.Constraints
import Postmap.Database.Names
import Postmap.Database.Reference
import Postmap.Database.Structure
import Postmap.Database.Types


-- * Instrospection


introspect :: DatabaseName -> SchemaName -> Hasql.Connection.Connection -> IO Database
introspect name schema conn = do
  structure <- getStructure schema conn
  pure
    MkDatabase
      { databaseName = name
      , databaseSchema = buildSchema schema structure
      }


buildSchema :: SchemaName -> DbStructure -> Schema
buildSchema schema structure@MkDbStructure {..} =
  MkSchema
    { schemaName = schema
    , schemaTables =
        buildTables
          dbStructureTables
          schema
          structure
    }


buildTables :: [DbTable] -> SchemaName -> DbStructure -> [Table]
buildTables [] _ _ = []
buildTables (table : tables) schema structure =
  buildTable table schema structure : buildTables tables schema structure


buildTable :: DbTable -> SchemaName -> DbStructure -> Table
buildTable MkDbTable {..} schema structure =
  let tableName = dbTableTableName
      tableIsView = dbTableIsView
      tableColumns = buildColumns tableName schema structure
      tablePrimaryKey = buildPrimaryKey tableName schema structure
      tableForeignKeys = buildForeignKeys tableName schema structure
      tableUniqueConstraints = buildUniqueConstraints tableName schema structure
   in MkTable {..}


buildColumns :: TableName -> SchemaName -> DbStructure -> [Column]
buildColumns tableName schema MkDbStructure {..} =
  let fp MkDbTableColumn {..} = dbTableColumnTableName == tableName && dbTableColumnSchemaName == schema
      columns = filter fp dbStructureColumns
   in flip fmap columns $ \MkDbTableColumn {..} ->
        MkColumn
          { columnName = dbTableColumnColumnName
          , columnType = dbTableColumnType
          , columnNullable = dbTableColumnNullable
          }


buildPrimaryKey :: TableName -> SchemaName -> DbStructure -> Maybe PrimaryKey
buildPrimaryKey tableName schema MkDbStructure {..} =
  let fp MkDbTableColumnReference {..} = dbTableColumnReferenceTableName == tableName && dbTableColumnReferenceSchemaName == schema
      primaryKeys = filter fp dbStructurePrimaryKeys
   in case primaryKeys of
        [] -> Nothing
        [s] -> Just . PrimaryKeyS $ dbTableColumnReferenceColumnName s
        xs -> Just . PrimaryKeyC . NE.fromList $ fmap dbTableColumnReferenceColumnName xs


buildForeignKeys :: TableName -> SchemaName -> DbStructure -> [ForeignKey]
buildForeignKeys tableName schema MkDbStructure {..} =
  let fp MkDbForeignKey {..} = dbForeignKeyTableName == tableName && dbForeignKeyTableSchemaName == schema
      foreignKeys = filter fp dbStructureForeignKeys
      gForeignKeys = groupWith dbForeignKeyConstraintName foreignKeys
   in flip fmap gForeignKeys $ \fks ->
        let foreignKeyColumns = flip fmap fks $ \MkDbForeignKey {..} ->
              MkForeignKeyColumnPair
                { foreignKeyColumnPairSource = dbForeignKeyColumnName
                , foreignKeyColumnPairTarget =
                    MkColumnReference
                      { columnReferenceSchema = dbForeignKeyForeignTableSchemaName
                      , columnReferenceTable = dbForeignKeyForeignTableName
                      , columnReferenceColumn = dbForeignKeyForeignColumnName
                      }
                }
         in MkForeignKey {..}


buildUniqueConstraints :: TableName -> SchemaName -> DbStructure -> [UniqueConstraint]
buildUniqueConstraints tableName schema MkDbStructure {..} =
  let fp MkDbUniqueConstraint {..} = dbUniqueConstraintTableName == tableName && dbUniqueConstraintSchemaName == schema
      uniqueConstraints = filter fp dbStructureUniqueConstraints
   in flip fmap uniqueConstraints $ \MkDbUniqueConstraint {..} ->
        case dbUniqueConstraintColumns of
          a NE.:| [] -> UniqueConstraintS a
          xs -> UniqueConstraintC xs


-- * Database Structure


data DbStructure = MkDbStructure
  { dbStructureTables :: [DbTable]
  , dbStructureColumns :: [DbTableColumn]
  , dbStructurePrimaryKeys :: [DbTableColumnReference]
  , dbStructureForeignKeys :: [DbForeignKey]
  , dbStructureUniqueConstraints :: [DbUniqueConstraint]
  }
  deriving (Eq, Show)


getStructure :: SchemaName -> Hasql.Connection.Connection -> IO DbStructure
getStructure schema conn =
  Hasql.Session.run (sessStructure schema) conn >>= \case
    Left err -> error (show err)
    Right inspection -> pure inspection


sessStructure :: SchemaName -> Hasql.Session.Session DbStructure
sessStructure schema = do
  tables <- Hasql.Session.statement schema stmtTables
  columns <- Hasql.Session.statement schema stmtColumns
  primaryKeys <- Hasql.Session.statement schema stmtPrimaryKeys
  foreignKeys <- Hasql.Session.statement schema stmtForeignKeys
  uniqueConstraints <- Hasql.Session.statement schema stmtUniqueConstraints
  pure
    MkDbStructure
      { dbStructureTables = V.toList tables
      , dbStructureColumns = V.toList columns
      , dbStructurePrimaryKeys = V.toList primaryKeys
      , dbStructureForeignKeys = V.toList foreignKeys
      , dbStructureUniqueConstraints = V.toList uniqueConstraints
      }


-- * List Tables of a Schema


data DbTable = MkDbTable
  { dbTableSchemaName :: SchemaName
  , dbTableTableName :: TableName
  , dbTableIsView :: Bool
  }
  deriving (Eq, Show)


stmtTables :: Hasql.Statement.Statement SchemaName (V.Vector DbTable)
stmtTables =
  dimap schemaNameToText (fmap fromRow) stmt
  where
    stmt =
      [Hasql.TH.vectorStatement|
        SELECT "t"."table_schema" :: text
             , "t"."table_name" :: text
             , ("t"."table_type" = 'VIEW') :: bool
          FROM "information_schema"."tables" as "t"
         WHERE "t"."table_schema" = $1 :: text
           AND "t"."table_type" IN ('BASE TABLE', 'VIEW')
      |]
    fromRow (schema, name, isView) =
      MkDbTable
        { dbTableSchemaName = MkSchemaName (MkIdentifier schema)
        , dbTableTableName = MkTableName (MkIdentifier name)
        , dbTableIsView = isView
        }


-- * List Columns of All Tables


data DbTableColumn = MkDbTableColumn
  { dbTableColumnSchemaName :: SchemaName
  , dbTableColumnTableName :: TableName
  , dbTableColumnColumnName :: ColumnName
  , dbTableColumnNullable :: Bool
  , dbTableColumnType :: PgType
  }
  deriving (Eq, Show)


stmtColumns :: Hasql.Statement.Statement SchemaName (V.Vector DbTableColumn)
stmtColumns =
  dimap schemaNameToText (fmap fromRow) stmt
  where
    stmt =
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
    fromRow (schema, table, column, nullable, pgType) =
      MkDbTableColumn
        { dbTableColumnSchemaName = MkSchemaName (MkIdentifier schema)
        , dbTableColumnTableName = MkTableName (MkIdentifier table)
        , dbTableColumnColumnName = MkColumnName (MkIdentifier column)
        , dbTableColumnNullable = nullable
        , dbTableColumnType = either (error . ("Error parsing PGTYPE: " <>)) id (mkPgType (revisitPgType pgType))
        }
    -- TODO: How about multidimensional arrays?
    revisitPgType t =
      if T.isPrefixOf "_" t
        then T.drop 1 t <> "[]"
        else t


-- * List Primary Keys of All Tables


data DbTableColumnReference = MkDbTableColumnReference
  { dbTableColumnReferenceSchemaName :: SchemaName
  , dbTableColumnReferenceTableName :: TableName
  , dbTableColumnReferenceColumnName :: ColumnName
  }
  deriving (Eq, Show)


stmtPrimaryKeys :: Hasql.Statement.Statement SchemaName (V.Vector DbTableColumnReference)
stmtPrimaryKeys =
  dimap schemaNameToText (fmap fromRow) stmt
  where
    -- TODO: Ensure correct order of columns constituting the primary key.
    stmt =
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
    fromRow (schema, table, column) =
      MkDbTableColumnReference
        { dbTableColumnReferenceSchemaName = MkSchemaName (MkIdentifier schema)
        , dbTableColumnReferenceTableName = MkTableName (MkIdentifier table)
        , dbTableColumnReferenceColumnName = MkColumnName (MkIdentifier column)
        }


-- * List Foreign Keys of All Tables


data DbForeignKey = MkDbForeignKey
  { dbForeignKeyConstraintName :: T.Text
  , dbForeignKeyTableSchemaName :: SchemaName
  , dbForeignKeyTableName :: TableName
  , dbForeignKeyColumnName :: ColumnName
  , dbForeignKeyForeignTableSchemaName :: SchemaName
  , dbForeignKeyForeignTableName :: TableName
  , dbForeignKeyForeignColumnName :: ColumnName
  }
  deriving (Eq, Show)


stmtForeignKeys :: Hasql.Statement.Statement SchemaName (V.Vector DbForeignKey)
stmtForeignKeys =
  dimap schemaNameToText (fmap fromRow) stmt
  where
    stmt =
      -- TODO: Ensure correct order of columns constituting the foreign key.
      [Hasql.TH.vectorStatement|
        SELECT "tc"."constraint_name" :: text as "constraint_name"
             , "tc"."table_schema" :: text as "table_schema"
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
    fromRow (constraint, schema, table, column, foreignSchema, foreignTable, foreignColumn) =
      MkDbForeignKey
        { dbForeignKeyConstraintName = constraint
        , dbForeignKeyTableSchemaName = MkSchemaName (MkIdentifier schema)
        , dbForeignKeyTableName = MkTableName (MkIdentifier table)
        , dbForeignKeyColumnName = MkColumnName (MkIdentifier column)
        , dbForeignKeyForeignTableSchemaName = MkSchemaName (MkIdentifier foreignSchema)
        , dbForeignKeyForeignTableName = MkTableName (MkIdentifier foreignTable)
        , dbForeignKeyForeignColumnName = MkColumnName (MkIdentifier foreignColumn)
        }


-- * List Unique Constraints of All Tables


data DbUniqueConstraint = MkDbUniqueConstraint
  { dbUniqueConstraintName :: T.Text
  , dbUniqueConstraintSchemaName :: SchemaName
  , dbUniqueConstraintTableName :: TableName
  , dbUniqueConstraintColumns :: NE.NonEmpty ColumnName
  }
  deriving (Eq, Show)


stmtUniqueConstraints :: Hasql.Statement.Statement SchemaName (V.Vector DbUniqueConstraint)
stmtUniqueConstraints =
  dimap schemaNameToText (fmap fromRow) stmt
  where
    -- TODO: Ensure correct order of columns constituting the unique constraint.
    stmt =
      [Hasql.TH.vectorStatement|
        SELECT "c"."conname" :: text AS "constraint_name"
             , $1 :: text as "table_schema"
             , "c"."conrelid"::regclass :: text AS "table_name"
             , array_agg("a"."attname" :: text ORDER BY "k"."n") :: text[] AS "columns"
          FROM "pg_constraint" AS "c"
               CROSS JOIN LATERAL unnest("c"."conkey") WITH ORDINALITY AS k("c", "n")
               JOIN "pg_attribute" AS "a" ON "a"."attnum" = "k"."c"
                AND "a"."attrelid" = "c"."conrelid"
         WHERE "c"."contype" = 'u'
           AND "c"."connamespace" = $1 :: text :: regnamespace
      GROUP BY "c"."oid", "c"."conrelid", "c"."conname"
      |]
    fromRow (constraint, schema, table, columns) =
      MkDbUniqueConstraint
        { dbUniqueConstraintName = constraint
        , dbUniqueConstraintSchemaName = MkSchemaName (MkIdentifier schema)
        , dbUniqueConstraintTableName = MkTableName (MkIdentifier table)
        , dbUniqueConstraintColumns = NE.fromList . V.toList $ fmap (MkColumnName . MkIdentifier) columns
        }
