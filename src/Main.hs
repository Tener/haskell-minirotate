module Main where

import System.Time
import System.FilePath
import System.Directory
import System.Environment
import System.IO ( hPutStrLn, stderr )
import System.Exit ( exitFailure )

import Data.Accessor
import Data.Accessor.Tuple
import Data.Accessor.Basic ( get )
import Data.Maybe
import Data.List ( sort, partition, intercalate )

import Control.Monad ( when )
import Control.Applicative ( (<$>) )

import Text.Printf

import Paths_minirotate ( version )

import Debug.Trace

-- local imports
import Options
import Pattern

data FileOrDir = File | Dir deriving (Show,Ord,Eq)

-- | auxilary function. to be replaced by proper logging.
logErr str = hPutStrLn stderr ("ERR: " ++ str)

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



copyOp :: CopyMode -> (FilePath -> FilePath -> IO ())
-- copyOp Copy = \from to -> logErr (printf "COPY: %s -> %s" from to)
-- copyOp Move = \from to -> logErr (printf "MOVE: %s -> %s" from to)
copyOp Copy = copyFile
copyOp Move = renameFile

runRotate :: RunOptions -> [FilePath] -> IO ()
runRotate opts locs | length locs < 2 = logErr "Need at least one source location and destination\n" >> exitFailure
                    | otherwise = do

  -- check pattern
  runPattern <- case compilePattern (get filePattern $ opts) of
                  Left err -> logErr ("Pattern error: " ++ err) >> exitFailure
                  Right pat -> return pat

  let -- if source location is empty string "" we skip it.
      locFrom = (filter (not . null)) $ init locs
      locTo = last locs

      -- | oneLocation firstLevel? (element type, element name)
      oneLocation _ (File,fp) = do
        modTime <- getModificationTime fp
        (copyOp (get copyMode $ opts)) fp (locTo </> (runPattern modTime fp))
      oneLocation True (Dir,fp) = do
        logErr (printf "Directory %s is a subdirectory at source site. Recursive operation is not supported. Skipping."
                        (show fp))
      oneLocation False (Dir,fp) = do
        cont <- getDirectoryContentsRooted fp
        mapM fileOrDir cont >>= mapM_ (either logErr (oneLocation True))

  -- check for correct destination
  dest <- fileOrDir locTo
  case dest of
    Left err -> logErr ("Problem with destination:" ++ err) >> exitFailure
    Right (File,fp) -> logErr ("Destination should be directory, not file: " ++ (show fp))  >> exitFailure
    _ -> return () -- all ok.

  -- for each location decide if it's file or directory.
  -- then copy file to destination or report error.
  mapM fileOrDir locFrom >>= mapM_ (either logErr (oneLocation False))

  -- rotate files in final directory
  rotateFiles opts locTo

-- | rotate files in location directory
rotateFiles :: RunOptions -> FilePath -> IO ()
rotateFiles opts loc = do
  -- time stuff
  now <- getClockTime

  -- get files in directory, filter out directories
  let checkFileGetModTime (Left err) = logErr ("rotateFiles: bad file: " ++ err) >> return Nothing
      checkFileGetModTime (Right (Dir,fp)) = logErr ("rotateFiles: skipping directory: " ++ show fp) >> return Nothing
      checkFileGetModTime (Right (File,fp)) = do
                         mt <- getModificationTime fp
                         return (Just (fp,mt))

  -- important note: files are sorted by (modification time,name)
  cont <- sort <$> (mapM fileOrDir =<< getDirectoryContentsRooted loc)
  contM <- catMaybes <$> mapM checkFileGetModTime cont

  -- now apply the rules (in order):
  -- 1. Delete too old files (by mod time)
  -- 2. Delete exceeding files (by filename)
  -- 3. Undelete to keep minimum count of files (by filename)

  let -- here is the implementation of rules. note the nice symmetry.
      -- 1.
      maxAge = addToClockTime (noTimeDiff { tdSec = negate (get maximumAge $ opts) }) now
      tooOld (todel,tokeep) = let (newdel,keep) = partition (\(fp,mt) -> mt < maxAge) tokeep
                              in {- tS 1 -}  ((newdel ++ todel),keep)
      -- 2.
      tooMany (todel,tokeep) = let (newdel,keep) = splitAt (length contM - (get maximumFiles $ opts)) (sort tokeep)
                               in {- tS 2 -} ((newdel ++ todel),keep)
      -- 3.
      keepMin (todel,tokeep) = let (del,newkeep) = splitAt (length contM - (get minimumFiles $ opts)) (sort todel)
                               in {- tS 3 -} (del,newkeep ++ tokeep)

      getDeletedFiles = map fst . fst
      getKeptFiles = map fst . snd
      partitionedFiles = (keepMin . tooMany . tooOld) $ ([],contM)

{-
  print ("maxAge",maxAge)
  print ("now",now)

  putStrLn "DELETE:"
  print (getDeletedFiles partitionedFiles)
  putStrLn "KEEP:"
  print (getKeptFiles partitionedFiles)
-}

  mapM_ removeFile (getDeletedFiles partitionedFiles)

  return ()


-- | * A few auxilary functions

getDirectoryContentsRooted loc = map (loc </>) . filterSpecial <$> getDirectoryContents loc
-- | remove special directory elements ('.','..')
filterSpecial = (filter (\fp -> not (fp `elem` [".",".."])))
fileOrDir fp = do
  f <- doesFileExist fp
  d <- doesDirectoryExist fp
  return $ case () of
    _ | f -> Right (File,fp)
      | d -> Right (Dir,fp)
      | otherwise -> Left (printf "Neither file nor directory: %s" (show fp))


-- debug stuff
tS tag xs = let mapf' = showX . map fst
                mapf  = showY
            in
             traceShow (printf "%d. DEL: %s KEEP: %s"
                              (tag :: Int)
                              (mapf . fst $ xs)
                              (mapf . snd $ xs)
                       :: String
                      )
                      xs

showY xs = "[" ++ intercalate ", " (map (\(n,t) -> "(" ++ n ++ "," ++ show t ++ ")" ) xs) ++ "]"
showX xs = "[" ++ intercalate ", " xs ++ "]"