module Zamazingo.Aeson where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Text as T


keyParserFromEitherText :: String -> (T.Text -> Either String a) -> Aeson.FromJSONKeyFunction a
keyParserFromEitherText n f =
  Aeson.FromJSONKeyValue $ parseTextFromEither n f


parseTextFromEither :: String -> (T.Text -> Either String a) -> Aeson.Value -> Aeson.Types.Parser a
parseTextFromEither n f =
  Aeson.Types.withText n $ \t -> case f t of
    Left e -> fail e
    Right x -> pure x
