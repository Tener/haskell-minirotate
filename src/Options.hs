module Options where

import System.Console.GetOpt

data CopyMode = Move 
              | Copy
                deriving (Read,Show,Eq,Ord)

data RunOptions = RunOptions { 
      sourceDir   :: Filepath     -- ^ take files from this directory
    , destDir     :: Filepath     -- ^ put files in this directory
    , filePattern :: String       -- ^ output file pattern
    , copyMode    :: CopyMode     -- ^ should we move or copy files?
    }

data EnvOptions = EnvOptions {
      optShowHelp :: Bool,
      optShowVers :: Bool,
      -- NOT IMPLEMENTED YET:
      optLogger   :: (),
      optVerbose  :: ()
    }

class Default a where
    def :: a

instance Default RunOptions where
    def = RunOptions {
            sourceDir   = "."
          , destDir     = "."
          , filePattern = "{basename}-{date %Y-%m-%d}.{ext}"
          , copyMode    = Copy
          }

instance Default EnvOptions where
    def = EnvOptions {
            
          }



options :: [OptDescr (Options -> Options)]
options = 
    [ Option ['V','?']

