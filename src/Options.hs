{-# LANGUAGE TemplateHaskell #-}

module Options where

import System.Console.GetOpt
import Data.Accessor
import Data.Accessor.Tuple
import Data.Accessor.Template
import Text.Printf

import Safe ( readDef )

data CopyMode = Move | Copy
                deriving (Read,Show,Eq,Ord)

data RunOptions = RunOptions { 
      filePattern_  :: String       -- ^ output file pattern
    , copyMode_     :: CopyMode     -- ^ should we move or copy files?
    , minimumFiles_ :: Int          -- ^ minimum # of files
    , maximumFiles_ :: Int          -- ^ maximum # of files
    , maximumAge_   :: Int          -- ^ oldest age allowed (in seconds)
    , dryRun_       :: Bool         -- ^ should we really commit our operations?
    , externalMove_ :: Bool         -- ^ use 'mv' command instead of renameFile
    , externalCopy_ :: Bool         -- ^ use 'cp' command instead of copyFile
    } deriving (Read,Show,Eq,Ord)

                         
data EnvOptions = EnvOptions {
      showHelp_ :: Bool -- ^ show help
    , showVers_ :: Bool -- ^ show version on run
    , showDefs_ :: Bool -- ^ show program defaults
    , continue_ :: Bool -- ^ set to False if either showVers or showHelp is True. if True program will not be run.

--    NOT IMPLEMENTED YET:
    , logger_   :: String
    , verbose_  :: Bool
    } deriving (Read,Show,Eq,Ord)

class Default a where
    def :: a

instance Default RunOptions where
    def = RunOptions {
            filePattern_  = "{basename}-{modtime %d-%m-%Y-%H_%M_%S}{ext}" -- ext includes leading '.'
          , copyMode_     = Copy
          , minimumFiles_ = 3
          , maximumFiles_ = 20
            -- set maximum age to 3 months
          , maximumAge_   = 3600 {- hour -} * 24 {- day -} * 30 {- month -} * 3
          , dryRun_       = False
          , externalMove_ = False
          , externalCopy_ = False
          }

instance Default EnvOptions where
    def = EnvOptions {
            showHelp_ = False
          , showVers_ = False
          , showDefs_ = False
          , continue_ = True
          , logger_   = ""
          , verbose_  = False
          }

type Options = (EnvOptions,RunOptions)
type ErrorString = String

$( deriveAccessors ''RunOptions )
$( deriveAccessors ''EnvOptions )

-- | strict function composition
f .! g = \x -> f $! g x
readErr errmsg str = readDef (error (printf "%s (input: %s)" errmsg str)) str

options :: [OptDescr (Options -> Options)]
options = [ Option ['h','?'] ["help"] (NoArg ((first .> continue ^= False) . 
                                              (first .> showHelp ^= True)))
                   "show help"
          , Option ['V'] ["version"] (NoArg ((first .> continue ^= False) .
                                              (first .> showVers ^= True)))
                   "show version"
          , Option [] ["show-defaults"] (NoArg ((first .> continue ^= False) .
                                                (first .> showDefs ^= True)))
                   "show program defaults"
          , Option ['p'] ["pattern"] (ReqArg (second .> filePattern ^=) "PATTERN")
                   "pattern for final files"
          , Option ['m'] ["move"] (NoArg (second .> copyMode ^= Move))
                   "set copy mode to 'move'"
          , Option ['c'] ["copy"] (NoArg (second .> copyMode ^= Copy))
                   "set copy mode to 'copy'"
          , Option [] ["min-files"] (ReqArg ((second .> minimumFiles ^=) .! 
                                             (readErr "--min-files: bad format"))
                                             "NUM")
                   "minimum number of files to keep"
          , Option [] ["max-files"] (ReqArg ((second .> maximumFiles ^=) .! 
                                             (readErr "--max-files: bad format"))
                                             "NUM")
                   "maximum number of files to keep"
          , Option [] ["max-age"] (ReqArg ((second .> maximumAge ^=) .! 
                                           (readErr "--max-age: bad format"))
                                           "NUMSEC")
                   "maximum age of files to keep"
          , Option [] ["dry-run"] (NoArg (second .> dryRun ^= True))
                   "don't remove or copy any files"
          , Option [] ["external-move"] (NoArg (second .> externalMove ^= True))
                   "use external move impelmentation ('mv' command) instead of System.Directory.renameFile"
          , Option [] ["external-copy"] (NoArg (second .> externalCopy ^= True))
                   "use external move impelmentation ('cp' command) instead of System.Directory.copyFile"
--          TODO: logger, verbose
          ]

usage = usageInfo header options where
    header = "Usage: minirotate [OPTIONS] SOURCE [SOURCE..] DESTINATION"
                                     

parseOptions :: [String] -> Either ErrorString ([FilePath],Options)
parseOptions args = case getOpt Permute options args of
                               (o,places,[]) -> Right $ (places, foldl (flip id) (def,def) o)
                               (_,_,err) -> 
                                 let errs = if not (null err) 
                                             then "Bad options: \n" ++ concat err ++ "\n" 
                                             else ""
                                 in
                                   Left $ errs ++ usage
