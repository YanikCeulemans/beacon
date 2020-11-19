module Main where

import Prelude

import Beacon (annotate)
import Cli (annotateInput, parseAnnotateCliOptions, provideArgs)
import Data.Either (either)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Options.Applicative (handleParseResult)

main :: Effect Unit
main = do
  args <- provideArgs
  { annotateConfig, inputSrc } <- handleParseResult $ parseAnnotateCliOptions args
  launchAff_ do
    input <- annotateInput inputSrc
    either
      (liftEffect <<< log)
      (liftEffect <<< log <<< annotate annotateConfig)
      input
