module Main where

import Prelude

import Beacon (annotate)
import Cli (annotateInput, parseAnnotateCliOptions, provideArgs)
import Data.Either (either)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (error, log)
import Node.Process (exit)
import Options.Applicative (handleParseResult)

errorAndExit :: String -> Aff Unit
errorAndExit errorMsg = liftEffect do
  error errorMsg
  exit 1

main :: Effect Unit
main = do
  args <- provideArgs
  { annotateConfig, inputSrc } <- handleParseResult $ parseAnnotateCliOptions args
  launchAff_ do
    input <- unwrap $ annotateInput inputSrc
    either
      errorAndExit
      (liftEffect <<< log <<< annotate annotateConfig)
      input
