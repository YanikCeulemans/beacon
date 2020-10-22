module Main where

import Prelude

import Beacon (annotate, characterLocation, defaultConfig)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Node.Process (argv)

main :: Effect Unit
main = do
  cmdLineArgs <- argv
  logShow cmdLineArgs
  log $ annotate defaultConfig (characterLocation 3 4) "line 1\nline 2\nline 3\nline 4"
