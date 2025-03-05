{-# LANGUAGE OverloadedStrings #-}

-- | This module provides top-level definitions for the CLI program.
module Postmap.Cli where

import qualified Autodocodec.Yaml as ADC.Yaml
import Control.Applicative ((<**>), (<|>))
import Control.Monad (join)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Hasql.Connection
import qualified Options.Applicative as OA
import qualified Postmap.Codegen.Haskell.Rel8
import qualified Postmap.Database as PD
import qualified Postmap.Diagrams as Diagrams
import qualified Postmap.Meta as Meta
import System.Exit (ExitCode (..))
import qualified Zamazingo.Text as Z.Text


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
    <|> commandDiagrams
    <|> commandGencode
    <|> commandVersion


-- * Commands


-- ** introspect


-- | Definition for @introspect@ CLI command.
commandIntrospect :: OA.Parser (IO ExitCode)
commandIntrospect =
  OA.hsubparser (OA.command "introspect" (OA.info parser infomod) <> OA.metavar "introspect")
  where
    infomod =
      OA.fullDesc
        <> infoModHeader
        <> OA.progDesc "Introspect database schema."
        <> OA.footer "This command introspects a database schema and produces its structure."
    parser =
      doIntrospect
        <$> OA.strOption
          ( OA.short 'u'
              <> OA.long "uri"
              <> OA.help "Database connection URI."
          )
        <*> OA.strOption
          ( OA.short 's'
              <> OA.long "schema"
              <> OA.value "public"
              <> OA.showDefault
              <> OA.help "Database schema to introspect."
          )
        <*> OA.option
          optParseOutputFormat
          ( OA.short 'f'
              <> OA.long "format"
              <> OA.value OutputFormatJson
              <> OA.showDefault
              <> OA.help "Output format."
          )


-- | @introspect@ CLI command program.
doIntrospect :: B.ByteString -> T.Text -> OutputFormat -> IO ExitCode
doIntrospect u s f = do
  eConn <- Hasql.Connection.acquire u
  case eConn of
    Left err -> do
      TIO.putStrLn ("Error while connecting to database: " <> Z.Text.tshow err)
      pure (ExitFailure 1)
    Right conn -> do
      let dName = PD.MkDatabaseName (PD.MkIdentifier "database")
      let sName = PD.MkSchemaName (PD.MkIdentifier s)
      tables <- PD.introspect dName sName conn
      case f of
        OutputFormatJson -> BLC.putStrLn (Aeson.encode tables)
        OutputFormatYaml -> BC.putStrLn (ADC.Yaml.encodeYamlViaCodec tables)
      pure ExitSuccess


-- ** diagrams


-- | Definition for @diagrams@ CLI command.
commandDiagrams :: OA.Parser (IO ExitCode)
commandDiagrams =
  OA.hsubparser (OA.command "diagrams" (OA.info parser infomod) <> OA.metavar "diagrams")
  where
    infomod =
      OA.fullDesc
        <> infoModHeader
        <> OA.progDesc "Produce Diagrams."
        <> OA.footer "This command produces diagrams."
    parser =
      doDiagrams
        <$> OA.strOption
          ( OA.short 's'
              <> OA.long "schema"
              <> OA.help "Path to the database schema file."
          )
        <*> OA.strOption
          ( OA.short 'd'
              <> OA.long "directory"
              <> OA.help "Path to the output directory."
          )


doDiagrams :: FilePath -> FilePath -> IO ExitCode
doDiagrams fp dp = do
  eSchema <- ADC.Yaml.eitherDecodeYamlViaCodec <$> B.readFile fp
  case eSchema of
    Left err -> do
      TIO.putStrLn ("Error while parsing schema file: " <> Z.Text.tshow err)
      pure (ExitFailure 1)
    Right schema -> do
      Diagrams.runDiagrams dp schema
      pure ExitSuccess


-- ** gencode


-- | Definition for @gencode@ CLI command.
commandGencode :: OA.Parser (IO ExitCode)
commandGencode =
  OA.hsubparser (OA.command "gencode" (OA.info parser infomod) <> OA.metavar "gencode")
  where
    infomod =
      OA.fullDesc
        <> infoModHeader
        <> OA.progDesc "Code Generation Commands."
        <> OA.footer "This command provides code generationg commands."
    parser =
      commandGencodeHaskell


-- ** gencode haskell


-- | Definition for @gencode haskell@ CLI command.
commandGencodeHaskell :: OA.Parser (IO ExitCode)
commandGencodeHaskell =
  OA.hsubparser (OA.command "haskell" (OA.info parser infomod) <> OA.metavar "haskell")
  where
    infomod =
      OA.fullDesc
        <> infoModHeader
        <> OA.progDesc "Produce Haskell Code."
        <> OA.footer "This command produces Haskell code."
    parser =
      doGencodeHaskell
        <$> OA.strOption (OA.short 's' <> OA.long "schema" <> OA.help "Path to the schema file.")
        <*> OA.strOption (OA.short 'c' <> OA.long "config" <> OA.help "Path to the configuration file.")


doGencodeHaskell :: FilePath -> FilePath -> IO ExitCode
doGencodeHaskell fpS fpC = do
  eSchema <- ADC.Yaml.eitherDecodeYamlViaCodec <$> B.readFile fpS
  eConfig <- ADC.Yaml.eitherDecodeYamlViaCodec <$> B.readFile fpC
  case (eSchema, eConfig) of
    (Left err, _) -> do
      TIO.putStrLn ("Error while parsing schema file: " <> Z.Text.tshow err)
      pure (ExitFailure 1)
    (_, Left err) -> do
      TIO.putStrLn ("Error while parsing config file: " <> Z.Text.tshow err)
      pure (ExitFailure 1)
    (Right schema, Right config) -> do
      Postmap.Codegen.Haskell.Rel8.generate config schema
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


-- ** Output Formats


data OutputFormat
  = OutputFormatJson
  | OutputFormatYaml
  deriving (Bounded, Enum, Eq, Show)


avilableOutputFormats :: [OutputFormat]
avilableOutputFormats = [minBound .. maxBound]


outputFormatToString :: OutputFormat -> String
outputFormatToString OutputFormatJson = "json"
outputFormatToString OutputFormatYaml = "yaml"


outputFormatFromString :: String -> Either String OutputFormat
outputFormatFromString "json" = Right OutputFormatJson
outputFormatFromString "yaml" = Right OutputFormatYaml
outputFormatFromString s = Left ("Unknown output format: " <> s)


optParseOutputFormat :: OA.ReadM OutputFormat
optParseOutputFormat =
  OA.eitherReader outputFormatFromString
