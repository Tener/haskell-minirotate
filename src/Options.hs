{-# LANGUAGE TemplateHaskell #-}

module Options where

import System.Console.GetOpt
import Data.Accessor
import Data.Accessor.Tuple
import Data.Accessor.Template

data CopyMode = Move | Copy
                deriving (Read,Show,Eq,Ord)

data RunOptions = RunOptions { 
      sourceDir_   :: FilePath     -- ^ take files from this directory
    , destDir_     :: FilePath     -- ^ put files in this directory
    , filePattern_ :: String       -- ^ output file pattern
    , copyMode_    :: CopyMode     -- ^ should we move or copy files?
    }

data EnvOptions = EnvOptions {
      showHelp_ :: Bool -- ^ show help
    , showVers_ :: Bool -- ^ show version on run
    , continue_ :: Bool -- ^ set to False if either showVers or showHelp is True. if True program will not be run.

--    NOT IMPLEMENTED YET:
    , logger_   :: String
    , verbose_  :: Bool
    }

class Default a where
    def :: a

instance Default RunOptions where
    def = RunOptions {
            sourceDir_   = "."
          , destDir_     = "."
          , filePattern_ = "{basename}-{date %Y-%m-%d}.{ext}"
          , copyMode_    = Copy
          }

instance Default EnvOptions where
    def = EnvOptions {
            showHelp_ = False
          , showVers_ = False
          , continue_ = True
          , logger_   = ""
          , verbose_  = False
          }

type Options = (EnvOptions,RunOptions)

$( deriveAccessors ''RunOptions )
$( deriveAccessors ''EnvOptions )

options :: [OptDescr (Options -> Options)]
options = [ Option ['h','?'] ["help"] (NoArg ((first .> showHelp) ^= True)) "show help"
          , Option ['V'] ["version"] (NoArg ((first .> showVers) ^= True)) "show version"
          ]


{-
options :: [OptDescr (Options -> Options)]
options =
     [ Option ['v']     ["verbose"]
        (NoArg (\ opts -> opts { optVerbose = True }))
        "chatty output on stderr"
     , Option ['V','?'] ["version"]
        (NoArg (\ opts -> opts { optShowVersion = True }))
        "show version number"
     , Option ['o']     ["output"]
        (OptArg ((\ f opts -> opts { optOutput = Just f }) . fromMaybe "output")
        "FILE")
        "output FILE"
     , Option ['c']     []
        (OptArg ((\ f opts -> opts { optInput = Just f }) . fromMaybe "input")
        "FILE")
        "input FILE"
     , Option ['L']     ["libdir"]
        (ReqArg (\ d opts -> opts { optLibDirs = optLibDirs opts ++ [d] }) "DIR")
        "library directory"
     ]
-}