module Pattern where

import System.Time
import System.FilePath


-- TODO: fix this placeholder
compilePattern :: String -> Either String (ClockTime -> String -> String)
compilePattern pat = Right (\modtime fp -> takeFileName fp)
