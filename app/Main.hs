{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Logger (LogLevel (..), LogSource, filterLogger,
                                       runStderrLoggingT)
import           Data.Semigroup       ((<>))
import           Lib.Types            (Amplitude (..), StreamName (..))
import           Options.Applicative

reportArgsParser :: Parser (StreamName, Amplitude)
reportArgsParser =
  (,)
    <$> streamNameParser
    <*> amplitudeParser
  where
    streamNameParser = StreamName <$> strArgument   (metavar "STREAMNAME" <> help "Stream to report for")
    amplitudeParser  = Amplitude  <$> argument auto (metavar "AMPLITUDE"  <> help "Amplitude to use")

loggingFlag :: Parser (LogSource -> LogLevel -> Bool)
loggingFlag = option
                (maybeReader namedFilter)
                (short 'l'
                  <> long "logging"
                  <> help "specify logging level (default | debug | trace)"
                  <> metavar "LEVEL"
                  <> value defaultLogFilter
                  <> showDefaultWith (const "default")
                )
    where
      namedFilter "default" = Just defaultLogFilter
      namedFilter "debug"   = Just debugLogFilter
      namedFilter "trace"   = Just traceLogFilter
      namedFilter _         = Nothing

      defaultLogFilter _ (LevelOther "Trace") = False
      defaultLogFilter _ level                = level >= LevelInfo

      debugLogFilter "AWS Client" (LevelOther "Trace") = False
      debugLogFilter "AWS Client" level                = level >= LevelInfo
      debugLogFilter _            _                    = True

      traceLogFilter _ _                               = True

main :: IO ()
main = do
  (loggingFilter, reportArgs) <- customExecParser pref opts

  runStderrLoggingT . filterLogger loggingFilter $
    undefined reportArgs

  where
    pref = prefs showHelpOnEmpty
    opts = info (((,) <$> loggingFlag <*> reportArgsParser) <**> helper)
            (
              fullDesc
              <> progDesc "Report a measurement to STREAMNAME of AMPLITUDE"
              <> header "Ticker - a client for Binary Sunrise's Metronome"
            )
