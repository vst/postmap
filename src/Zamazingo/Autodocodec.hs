{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Zamazingo.Autodocodec where

import qualified Autodocodec as ADC
import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as Time
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import qualified Zamazingo.Text as Z.Text


-- | Date Codec.
dateCodec :: ADC.JSONCodec Time.Day
dateCodec =
  ADC.named "Date" codec
  where
    parse = maybe (Left "Invalid date value") pure . readMaybe . T.unpack
    codec =
      ADC.bimapCodec parse Z.Text.tshow ADC.textCodec
        ADC.<?> "Type representing date values in ISO 8601 format (YYYY-MM-DD)."


posixToUTCTimeCodec :: ADC.JSONCodec Time.UTCTime
posixToUTCTimeCodec =
  ADC.dimapCodec Time.posixSecondsToUTCTime Time.utcTimeToPOSIXSeconds ADC.codec


-- | Enum Codec.
boundedEnumCodec
  :: Enum enum
  => Eq enum
  => Bounded enum
  => (enum -> T.Text)
  -> ADC.JSONCodec enum
boundedEnumCodec ft =
  case NE.nonEmpty [minBound .. maxBound] of
    Nothing -> error "No enum values to construct JSONCodec via `boundedEnumCodec`." -- This should never happen.
    Just ss -> ADC.stringConstCodec (fmap (\v -> (v, ft v)) ss)


-- | Result type.
data Result e a
  = MkResultFailure !e
  | MkResultSuccess !a
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec (Result e a))


instance (ADC.HasCodec e, ADC.HasCodec a) => ADC.HasCodec (Result e a) where
  codec =
    ADC.object "Result" ADC.objectCodec


instance (ADC.HasCodec e, ADC.HasCodec a) => ADC.HasObjectCodec (Result e a) where
  objectCodec =
    ADC.discriminatedUnionCodec "type" enc dec
    where
      codecFailure = ADC.requiredField "value" "Failure."
      codecSuccess = ADC.requiredField "value" "Success."
      enc x = case x of
        MkResultFailure c -> ("failure", ADC.mapToEncoder c codecFailure)
        MkResultSuccess c -> ("success", ADC.mapToEncoder c codecSuccess)
      dec =
        [ ("failure", ("MkResultFailure", ADC.mapToDecoder MkResultFailure codecFailure))
        , ("success", ("MkResultSuccess", ADC.mapToDecoder MkResultSuccess codecSuccess))
        ]
