module Main where

import           Data.Semigroup      ((<>))
import           Lib.Types           (Amplitude (..), StreamName (..))
import           Options.Applicative

reportArgsParser :: Parser (StreamName, Amplitude)
reportArgsParser =
  (,)
    <$> streamNameParser
    <*> amplitudeParser
  where
    streamNameParser = StreamName <$> strArgument   (metavar "STREAMNAME" <> help "Stream to report for")
    amplitudeParser  = Amplitude  <$> argument auto (metavar "AMPLITUDE"  <> help "Amplitude to use")

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
