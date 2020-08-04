module Main

import CSV
import System.File
import System
import Data.Maybe

partial
main : IO ()
main = do [_, fileName] <- getArgs
          Right content <- readFile fileName
          case printCSV <$> (parseCSV content) of
               Just s => putStrLn s
               Nothing => pure ()
          pure ()

