module Main where

import Prelude

import Beacon (annotate)
import Cli (annotateInput, parseAnnotateConfig)
import Data.Either (either)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)

main :: Effect Unit
main = do
  { annotateConfig, inputSrc } <- parseAnnotateConfig
  launchAff_ do
    input <- annotateInput inputSrc
    either
      (liftEffect <<< log)
      (liftEffect <<< log <<< annotate annotateConfig)
      input
