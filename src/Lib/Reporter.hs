{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Lib.Reporter (reportMeasurement) where

import           Control.Lens                ((.~), (<&>), (^.))
import           Control.Monad               (void)
import           Control.Monad.Catch         (MonadCatch)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Logger
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import           Data.Aeson                  (encode)
import           Data.Binary.Builder         (toLazyByteString)
import           Data.ByteString.Lazy        (toStrict)
import           Data.Semigroup              ((<>))
import           Data.Text                   (Text, pack)
import           Data.Text.Encoding          (decodeUtf8)
import           Data.Time.Clock             (getCurrentTime)
import           Lib.Constants               (tier)
import           Lib.Types                   (Amplitude, GenericMetric (..),
                                              StreamName)
import           Network.AWS
import           Network.AWS.SQS
import           Network.AWS.STS

data TickerConfig = TickerConfig {
  sqsRegion    :: Region,
  sqsQueueName :: Text
} deriving Show

doAWS :: forall io. forall a. (MonadIO io, MonadCatch io, MonadBaseControl IO io, MonadLoggerIO io) => Region -> AWS a -> io a
doAWS region actions = do
  rawLogger <- askLoggerIO

  let lgr level msg = rawLogger defaultLoc "AWS Client" (levelConvert level) (toLogStr $ toLazyByteString msg)

  env <- newEnv Discover <&> envRegion .~ region <&> envLogger .~ lgr

  runResourceT $ runAWS env actions

  where
    levelConvert Trace = LevelOther "Trace"
    levelConvert Debug = LevelDebug
    levelConvert Info  = LevelInfo
    levelConvert Error = LevelError

sendSQS :: (MonadIO io, MonadCatch io, MonadBaseControl IO io, MonadLoggerIO io) => GenericMetric -> ReaderT TickerConfig io ()
sendSQS metric = do
  TickerConfig{..} <- ask

  void . doAWS sqsRegion $
    withQueue sqsQueueName $ \queueURL ->
      sendMessage queueURL messageData

  where
    messageData = decodeUtf8 . toStrict . encode $ [metric]
    withQueue name actionFn = do
      queueUrl <- send (getQueueURL name) <&> (^. gqursQueueURL)
      send $ actionFn queueUrl

genRuntimeConfig :: (MonadIO io, MonadCatch io, MonadBaseControl IO io, MonadLoggerIO io) => io (Either String TickerConfig)
genRuntimeConfig = do
  currentAccountInfo <- doAWS Oregon $ send getCallerIdentity

  logDebugNS "genRuntimeConfig" ("currentAccountInfo: " <> tshow currentAccountInfo)

  let currentAccountID = currentAccountInfo^.gcirsAccount

  case currentAccountID of
    Nothing ->
      return . Left $ "Could not determine AWS Account ID"
    Just acctID ->
      return . Right $ TickerConfig {
                          sqsRegion    = Oregon,
                          sqsQueueName = mkQueueName acctID
                        }

  where
    mkQueueName accountId = "metronome-" <> tier <> "-" <> accountId <> "-gmf"

reportMeasurement :: (MonadIO io, MonadCatch io, MonadBaseControl IO io, MonadLoggerIO io) => (StreamName, Amplitude) -> io ()
reportMeasurement (streamName, amplitude) = do
  timestamp <- liftIO getCurrentTime

  let measurement = GenericMetricV1{..}

  genRuntimeConfig >>= \case
    Left errmsg ->
      logErrorNS "reportMeasurement" (pack errmsg)
    Right runtimeConfig -> do
      logDebugNS "reportMeasurement" ("Config: " <> tshow runtimeConfig)
      runReaderT (sendSQS measurement) runtimeConfig

tshow :: Show a => a -> Text
tshow = pack . show
