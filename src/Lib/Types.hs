{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Lib.Types where

import           Data.Aeson      (ToJSON (..), object, (.=))
import           Data.Time.Clock (UTCTime)

newtype StreamName = StreamName String deriving (Eq, Show, ToJSON)
newtype Amplitude  = Amplitude  Double deriving (Eq, Show, ToJSON)

data GenericMetric = GenericMetricV1 {
  timestamp  :: UTCTime,
  amplitude  :: Amplitude,
  streamName :: StreamName
} deriving Show

instance ToJSON GenericMetric where
  toJSON GenericMetricV1{..} =
    object [
      "version"   .= (1 :: Integer),
      "timestamp" .= timestamp,
      "amplitude" .= amplitude,
      "stream"    .= streamName
    ]
