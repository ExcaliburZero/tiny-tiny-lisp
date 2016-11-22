module Main where

import System.Console.Haskeline
import Text.ParserCombinators.Parsec (parse)

import Interpreter.TinyTinyLisp.Parser

main :: IO ()
main = runInputT defaultSettings loop

loop :: InputT IO ()
loop = do
  minput <- getInputLine "% "
  case minput of
    Nothing    -> return ()
    Just ":q"  -> return ()
    Just input -> do
      outputStrLn $ processLine input
      loop

processLine :: String -> String
processLine line = case parse valueParser "" line of
  Right val -> show val
  Left  err -> show err
