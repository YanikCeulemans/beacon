module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Beacon (annotate, characterLocation, defaultConfig)

main :: Effect Unit
main = do
  log $ annotate defaultConfig (characterLocation 3 5) "line 1\nline 2\nline 3\nline 4"
