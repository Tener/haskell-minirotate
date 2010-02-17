module Main where

import System.Directory
import System.Environment
import System.IO ( hPutStrLn, stderr )
import System.Exit ( exitFailure )

import Data.Accessor
import Data.Accessor.Tuple
import Data.Accessor.Basic ( get )

import Control.Monad ( when )

import Text.Printf

import Paths_minirotate ( version )


import Options

data FileOrDir = File | Dir deriving Show

-- | auxilary function. to be replaced by proper logging.
logErr = hPutStrLn stderr

main :: IO ()
main = do
  args <- getArgs
  case parseOptions args of
    Left errmsg -> logErr errmsg >> exitFailure
    Right (locs,(envO,runO)) -> do
         when (get showHelp $ envO) (logErr usage)
         when (get showVers $ envO) (logErr $ printf "minirotate version %s" (show version))
         when (get showDefs $ envO) (logErr $ printf "Defaults:\n\t%s\n\t%s\n"
                                             (show (def :: EnvOptions))
                                             (show (def :: RunOptions)))
         when (get continue $ envO) (runRotate runO locs)

fileOrDir fp = do
  f <- doesFileExist fp
  d <- doesDirectoryExist fp
  return $ case () of
    _ | f -> Right (File,fp)
      | d -> Right (Dir,fp)
      | otherwise -> Left (printf "Neither file nor directory: %s" (show fp))

runRotate :: RunOptions -> [FilePath] -> IO ()
runRotate _ locs | length locs < 2 = logErr "Need at least one source location and destination\n" >> exitFailure
                 | otherwise = do
  let locFrom = init locs
      locTo = last locs
      oneLocation xxx = print xxx
      oneLocation (File,fp) = return ()
      oneLocation (Dir,fp) = return ()

  -- check for correct destination
  dest <- fileOrDir locTo
  case dest of
    Left err -> logErr ("Problem with destination:" ++ err) >> exitFailure
    Right (File,fp) -> logErr ("Destination should be directory, not file: " ++ (show fp))  >> exitFailure
    _ -> return () -- all ok.

  -- for each location decide if it's file or directory. 
  -- then copy file to destination or report error.
  mapM fileOrDir locFrom >>= mapM_ (either logErr oneLocation)

  -- rotate files in final directory
  -- rotateFiles locTo


