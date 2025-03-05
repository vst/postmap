{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides functions for generating diagrams from a database schema.
module Postmap.Diagrams where

import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Postmap.Database as PD
import qualified Zamazingo.D2 as Z.D2


-- | Renders a given database schema as a set of diagrams.
--
-- The diagrams are rendered in files under the given directory path.
--
-- 1. One for all tables in the schema.
-- 2. One for each table in the schema.
runDiagrams :: FilePath -> PD.Database -> IO ()
runDiagrams path PD.MkDatabase {..} = do
  let d2schema = schemaToD2 databaseSchema
  let d2tables = fmap (\t@PD.MkTable {..} -> (PD.tableNameToText tableName, tableToD2 t)) (PD.schemaTables databaseSchema)
  Z.D2.renderD2SqlTables (path <> "schema.svg") d2schema
  mapM_ (\(tName, t) -> Z.D2.renderD2SqlTables (path <> "table_" <> T.unpack tName <> ".svg") [t]) d2tables


schemaToD2 :: PD.Schema -> [Z.D2.D2SqlTable]
schemaToD2 PD.MkSchema {..} =
  fmap tableToD2 schemaTables


tableToD2 :: PD.Table -> Z.D2.D2SqlTable
tableToD2 table@PD.MkTable {..} =
  let d2SqlTableName = PD.tableNameToText tableName
      d2SqlTableColumns = fmap (columnToD2 table) tableColumns
   in Z.D2.MkD2SqlTable {..}


columnToD2 :: PD.Table -> PD.Column -> Z.D2.D2SqlColumn
columnToD2 PD.MkTable {..} PD.MkColumn {..} =
  let d2SqlColumnName = PD.columnNameToText columnName
      d2SqlColumnType = PD.pgTypeToText columnType
      d2SqlColumnPK = maybe False (PD.isColumnPartOfPrimaryKey columnName) tablePrimaryKey
      d2SqlColumnFK =
        (\PD.MkColumnReference {..} -> (PD.tableNameToText columnReferenceTable, PD.columnNameToText columnReferenceColumn))
          <$> mapMaybe (PD.findTargetReferenceForColumn columnName) tableForeignKeys
      d2SqlColumnUNQ = any (PD.isColumnUnique columnName) tableUniqueConstraints
   in Z.D2.MkD2SqlColumn {..}
