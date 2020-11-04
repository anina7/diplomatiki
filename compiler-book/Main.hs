module Main where

import qualified Automata
import qualified Grammars

main :: IO ()
main = do
  putStrLn $ replicate 72 '='
  putStrLn "Automata:"
  putStrLn $ replicate 72 '='
  Automata.main
  putStrLn ""
  putStrLn $ replicate 72 '='
  putStrLn "Grammars:"
  putStrLn $ replicate 72 '='
  Grammars.main
