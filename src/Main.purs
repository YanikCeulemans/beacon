module Main where

import Prelude

import Beacon (annotate, characterLocation)
import Cli (detectEncoding, parseAnnotateConfig, parseCharacterLocation, readFromStream)
import Data.Array (length)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Node.Buffer (toArray, toString)
import Node.Process (argv, stdin)

main :: Effect Unit
main = do
  parsedAnnotateConfig <- parseAnnotateConfig <$> argv
  parsedCharacterLocation <- parseCharacterLocation <$> argv
  either log identity $ do
    annotateCfg <- parsedAnnotateConfig
    characterLoc <- parsedCharacterLocation
    pure $ launchAff_ do
      maybeBuff <- readFromStream stdin
      case maybeBuff of
        Nothing ->
          liftEffect $ log "No stdin stream found"
        Just buff -> liftEffect do
          encoding <- detectEncoding <$> toArray buff
          t <- toString encoding buff
          log $ annotate annotateCfg characterLoc t
