{-# LANGUAGE OverloadedStrings #-}

-- | This module provides top-level definitions for the CLI program.
module Postmap.Cli where

import Control.Applicative ((<**>), (<|>))
import Control.Monad (join)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Hasql.Connection
import qualified Options.Applicative as OA
import qualified Postmap.Introspect as Introspect
import qualified Postmap.Meta as Meta
import System.Exit (ExitCode (..))


-- * Entrypoint


-- | CLI program entrypoint.
cli :: IO ExitCode
cli =
  join (OA.execParser (OA.info opts desc))
  where
    opts = optProgram <**> infoOptVersion <**> OA.helper
    desc =
      OA.fullDesc
        <> OA.progDesc "Top Level Commands"
        <> infoModHeader
        <> infoModFooter


-- * Program


-- | Option parser for top-level commands.
optProgram :: OA.Parser (IO ExitCode)
optProgram =
  commandIntrospect
    <|> commandVersion


-- * Commands


-- ** introspect


-- | Definition for @introspect@ CLI command.
commandIntrospect :: OA.Parser (IO ExitCode)
commandIntrospect = OA.hsubparser (OA.command "introspect" (OA.info parser infomod) <> OA.metavar "introspect")
  where
    infomod = OA.fullDesc <> infoModHeader <> OA.progDesc "Introspect database schema." <> OA.footer "This command introspect a database schema and produces its structure."
    parser =
      doIntrospect
        <$> OA.strOption (OA.short 'u' <> OA.long "uri" <> OA.help "Database connection URI.")
        <*> OA.strOption (OA.short 's' <> OA.long "schema" <> OA.value "public" <> OA.showDefault <> OA.help "Database schema to introspect.")


-- | @introspect@ CLI command program.
doIntrospect :: B.ByteString -> T.Text -> IO ExitCode
doIntrospect u s = do
  Right conn <- Hasql.Connection.acquire u
  tables <- Introspect.fetchSchema conn s
  BLC.putStrLn (Aeson.encode tables)
  pure ExitSuccess


-- ** version


-- | Definition for @version@ CLI command.
commandVersion :: OA.Parser (IO ExitCode)
commandVersion = OA.hsubparser (OA.command "version" (OA.info parser infomod) <> OA.metavar "version")
  where
    infomod = OA.fullDesc <> infoModHeader <> OA.progDesc "Show version and build information." <> OA.footer "This command shows version and build information."
    parser =
      doVersion
        <$> OA.switch (OA.short 'j' <> OA.long "json" <> OA.help "Format output in JSON.")


-- | @version@ CLI command program.
doVersion :: Bool -> IO ExitCode
doVersion True = BLC.putStrLn (Aeson.encode Meta.buildInfo) >> pure ExitSuccess
doVersion False = TIO.putStrLn (Meta.prettyBuildInfo Meta.buildInfo) >> pure ExitSuccess


-- * Helpers


-- | Version option parser.
infoOptVersion :: OA.Parser (a -> a)
infoOptVersion =
  OA.infoOption Meta.versionString $
    OA.short 'v'
      <> OA.long "version"
      <> OA.help "Show application version and exit"


-- | Header 'OA.InfoMod'.
infoModHeader :: OA.InfoMod a
infoModHeader =
  OA.header (T.unpack (Meta.name <> " - " <> Meta.title <> " v" <> Meta.versionText))


-- | Footer 'OA.InfoMod'.
infoModFooter :: OA.InfoMod a
infoModFooter =
  OA.footer "See <https://github.com/vst/postmap> for help and feedback."


-- | Tests a parser with given arguments.
runParserTest :: OA.Parser a -> [String] -> OA.ParserResult a
runParserTest parser = OA.execParserPure (OA.prefs prefs) (OA.info (parser <**> OA.helper) infomod)
  where
    prefs = OA.showHelpOnError <> OA.helpLongEquals <> OA.helpShowGlobals
    infomod = OA.fullDesc <> OA.progDesc "Test Parser" <> OA.header "testparser - especially for doctests"


-- | Tests an IO parser with given arguments.
runParserTestIO :: OA.Parser (IO a) -> [String] -> IO (Either String ())
runParserTestIO p as = case runParserTest p as of
  OA.Success _ -> pure (Right ())
  OA.Failure f -> pure (Left (show f))
  OA.CompletionInvoked _ -> pure (Right ())
