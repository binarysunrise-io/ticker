module Main where

import           Data.Semigroup      ((<>))
import           Options.Applicative

reportArgsParser :: Parser (String, Double)
reportArgsParser =
  (,)
    <$> streamNameParser
    <*> amplitudeParser
  where
    streamNameParser = strArgument   (metavar "STREAMNAME" <> help "Stream to report for")
    amplitudeParser  = argument auto (metavar "AMPLITUDE"  <> help "Amplitude to use")

main :: IO ()
main = customExecParser pref opts >>= print
  where
    pref = prefs showHelpOnEmpty
    opts = info (reportArgsParser <**> helper)
            (
              fullDesc
              <> progDesc "Report a measurement to STREAMNAME of AMPLITUDE"
              <> header "Ticker - a client for Binary Sunrise's Metronome"
            )
