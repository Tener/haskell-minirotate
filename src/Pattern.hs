{-# LANGUAGE TemplateHaskell #-}

module Pattern where

import System.Time
import System.FilePath
import System.Locale ( defaultTimeLocale )

import Data.Accessor
import Data.Accessor.Basic ( get )
import Data.Accessor.Template

import Data.List.Split ( split, dropBlanks, whenElt )
import Text.Printf

import Control.Monad.Error () -- brings instance for Monad (Either String b)
import Debug.Trace
import System.IO.Unsafe ( unsafePerformIO )

type ErrString = String

data PatternEnv = PatternEnv { file_     :: FilePath
                             , basename_ :: FilePath
                             , ext_      :: FilePath
                             , modtime_  :: ClockTime
                             }

$( deriveAccessors ''PatternEnv )

makeEnv mt fp =
    PatternEnv { file_     = takeFileName fp
               , basename_ = takeBaseName fp
               , ext_      = takeExtension fp
               , modtime_  = mt
               }

-- | compile file pattern and produce function that maps clock time and file name into resulting file name
compilePattern :: String -> Either ErrString (ClockTime -> String -> String)
compilePattern pat = case compilePatternLow pat of
                       Left err -> Left err
                       Right fun -> Right (\mt fp -> fun (makeEnv mt fp))

-- | compile pattern into environment-taking function that returns final string
compilePatternLow pat = let f <> g = \e -> (f e) ++ (g e)
                            aux acc [] = return acc
                            aux acc ("{":funargs:"}":rest) = do
                              let (fun,spaceargs) = break (==' ') funargs
                                  args = dropWhile (== ' ') spaceargs
                              newFun <- getFunction fun args
                              aux (acc <> newFun) rest
                            aux acc ("{":_) = fail "Bad format: unterminated '{'"
                            aux acc (sth:rest) = aux (acc <> const sth) rest
                        in
                            aux (const "") (split (whenElt (`elem` "{}")) pat)
                                                      

-- | return function that applied to environment will return string
getFunction "file" "" = Right (get file)
getFunction "ext" "" = Right (get ext)
getFunction "basename" "" = Right (get basename)
                                           
getFunction "file" _ = Left "Function 'file' takes no arguments."
getFunction "ext" _ = Left "Function 'ext' takes no arguments."
getFunction "basename" _ = Left "Function 'basename' takes no arguments."

getFunction "modtime" [] = Left "Function 'modtime' requires format string as an argument."
getFunction "modtime" format = Right (\ env -> let mt = (get modtime) $ env
                                                   ct = unsafePerformIO (toCalendarTime mt)
                                               in
                                                 formatCalendarTime defaultTimeLocale format ct)

getFunction xx yy = Left (printf "Unknown function: '%s', args: %s" xx (show yy))
