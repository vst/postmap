{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides some definitions for generating D2 diagrams.
--
-- A sample D2-based schema snippet is:
--
-- @@
-- costumes: {
--   shape: sql_table
--   id: int {constraint: primary_key}
--   silliness: int
--   monster: int
--   last_updated: timestamp
-- }
--
-- monsters: {
--   shape: sql_table
--   id: int {constraint: primary_key}
--   movie: string
--   weight: int
--   last_updated: timestamp
-- }
--
-- costumes.monster -> monsters.id
-- @@
--
-- >>> :set -XOverloadedStrings
-- >>>
-- >>> table = MkD2SqlTable "kvp" [MkD2SqlColumn "key" "text" True [] False, MkD2SqlColumn "value" "text" False [] False]
-- >>> tableToD2 table
-- "kvp: {\n  shape: sql_table\n  key: text {constraint: [primary_key]}\n  value: text\n}\n\n\n"
module Zamazingo.D2 where

import qualified Data.ByteString.Lazy as BL
import Data.String.Interpolate (i)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified System.Process.Typed as TP


data D2SqlTable = MkD2SqlTable
  { d2SqlTableName :: !T.Text
  , d2SqlTableColumns :: ![D2SqlColumn]
  }
  deriving (Eq, Show)


data D2SqlColumn = MkD2SqlColumn
  { d2SqlColumnName :: !T.Text
  , d2SqlColumnType :: !T.Text
  , d2SqlColumnPK :: !Bool
  , d2SqlColumnFK :: ![(T.Text, T.Text)]
  , d2SqlColumnUNQ :: !Bool
  }
  deriving (Eq, Show)


writeD2SqlTables :: FilePath -> [D2SqlTable] -> IO ()
writeD2SqlTables path =
  TIO.writeFile path . T.intercalate "\n\n" . fmap tableToD2


renderD2SqlTables :: FilePath -> [D2SqlTable] -> IO ()
renderD2SqlTables path =
  renderD2 path . T.intercalate "\n\n" . fmap tableToD2


renderD2 :: FilePath -> T.Text -> IO ()
renderD2 path d2 =
  TP.runProcess_
    . TP.setStdin (TP.byteStringInput . BL.fromStrict $ TE.encodeUtf8 d2)
    $ TP.proc "d2" ["--layout=elk", "-", path]


tableToD2 :: D2SqlTable -> T.Text
tableToD2 MkD2SqlTable {..} =
  let columns = fmap columnToD2 d2SqlTableColumns
      snipColumns = T.intercalate "\n  " columns
      snipForeignKeys = T.intercalate "\n" $ concatMap (columnFKsToD2 d2SqlTableName) d2SqlTableColumns
   in [i|#{d2SqlTableName}: {
  shape: sql_table
  #{snipColumns}
}

#{snipForeignKeys}
|]


columnToD2 :: D2SqlColumn -> T.Text
columnToD2 MkD2SqlColumn {..} =
  let constraints =
        ["primary_key" | d2SqlColumnPK]
          <> ["unique" | d2SqlColumnUNQ]
          <> ["foreign_key" | not (null d2SqlColumnFK)]
      snipConstraints = if null constraints then "" else " {constraint: [" <> T.intercalate "; " constraints <> "]}"
   in [i|#{sanitizeReserved d2SqlColumnName}: #{d2SqlColumnType}#{snipConstraints}|]


columnFKsToD2 :: T.Text -> D2SqlColumn -> [T.Text]
columnFKsToD2 tableName MkD2SqlColumn {..} =
  fmap (\(a, b) -> [i|#{tableName}.#{sanitizeReserved d2SqlColumnName} -> #{a}.#{sanitizeReserved b}|]) d2SqlColumnFK


sanitizeReserved :: T.Text -> T.Text
sanitizeReserved t =
  if t `elem` _reserved
    then t <> "_"
    else t


_reserved :: [T.Text]
_reserved =
  [ "class"
  , "classes"
  , "constraint"
  , "direction"
  , "grid-columns"
  , "grid-gap"
  , "grid-rows"
  , "height"
  , "horizontal-gap"
  , "icon"
  , "label"
  , "left"
  , "link"
  , "near"
  , "shape"
  , "style"
  , "tooltip"
  , "top"
  , "vertical-gap"
  , "width"
  ]
