{-# LANGUAGE TemplateHaskell #-}

module Options where

import System.Console.GetOpt
import Data.Accessor
import Data.Accessor.Tuple
import Data.Accessor.Template
import Text.Printf

data CopyMode = Move | Copy
                deriving (Read,Show,Eq,Ord)

data RunOptions = RunOptions { 
      filePattern_ :: String       -- ^ output file pattern
    , copyMode_    :: CopyMode     -- ^ should we move or copy files?
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
            filePattern_ = "{basename}-{date %Y-%m-%d}.{ext}"
          , copyMode_    = Copy
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
