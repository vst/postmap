module Main where

import qualified Postmap.Cli as Cli
import System.Exit (exitWith)


main :: IO ()
main = Cli.cli >>= exitWith
