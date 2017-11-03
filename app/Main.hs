{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.ByteString.Char8 as C8
import           Data.Semigroup             hiding (option)
import qualified Data.Set    as Set
import           Data.Time.Clock
import           Data.Version                          (showVersion)
import           Development.GitRev                    (gitCommitCount)
import           Distribution.System                   (buildArch)
import           Distribution.Text                     (display)
import           Options.Applicative.Simple
import qualified Paths_log_filter              as Meta

import Recogniser

-- ---------------------------------------------------------------------

main :: IO ()
main = do
    let commitCount = $gitCommitCount
        versionString' = concat $ concat
            [ [$(simpleVersion Meta.version)]
              -- Leave out number of commits for --depth=1 clone
              -- See https://github.com/commercialhaskell/stack/issues/792
            , [" (" ++ commitCount ++ " commits)" | commitCount /= ("1"::String) &&
                                                    commitCount /= ("UNKNOWN" :: String)]
            , [" ", display buildArch]
            ]
        numericVersion :: Parser (a -> a)
        numericVersion =
            infoOption
                (showVersion Meta.version)
                (long "numeric-version" <>
                 help "Show only version number")
    -- Parse the options and run
    (global, ()) <-
        simpleOptions
            versionString'
            "logfile-filter - Filter log lines that contain particular strings"
            ""
            (numericVersion <*> globalOptsParser)
            empty

    run global

-- ---------------------------------------------------------------------

data Settings = Settings
  { matchFile :: FilePath
  , logFile   :: FilePath
  } deriving Show

globalOptsParser :: Parser Settings
globalOptsParser = Settings
      <$> strOption
          ( long "matches"
         <> metavar "MATCHES"
         <> help "File containing the strings to match" )
      <*> strOption
          ( long "logfile"
         <> metavar "LOGFILE"
         <> help "Log file to be processed" )

-- ---------------------------------------------------------------------

run :: Settings -> IO ()
run (Settings matchesFile logFile) = do
  putStrLn $ "logfile:" ++ logFile
  matches <- readFile matchesFile
  logs    <- C8.readFile logFile
  let logLines = C8.lines logs

  putStrLn $ "matching:" ++ show (lines matches)
  putStrLn $ "log lines:" ++ show (length logLines)
  putStrLn $ "log lines:" ++ show (take 10 logLines)

  let t = makeMatcher (lines matches)
  let
    doOne (lineNum,line) =
      let !r = recogniser t line
      in if r == Set.empty then [] else [(lineNum,r)]

  tstart <- getCurrentTime
  let !r = concatMap doOne $ zip [0::Int ..] logLines
      !len = length r
  tend <- len `seq` getCurrentTime

  putStrLn $ unlines $ map show r
  putStrLn $ "Number of results:" ++ show len

  print $ diffUTCTime tend tstart

