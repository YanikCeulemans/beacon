module Cli where

import Prelude

import Beacon (AnnotateConfig, defaultConfig, withContextVertical, withoutLinenumbers)
import Data.Array (any, drop, dropWhile, last, take)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.Regex (parseFlags)

parseNaturalArg :: String -> Array String -> Either String (Maybe Int)
parseNaturalArg argName args =
  case dropWhile (\a -> a /= argName) args # take 2 # last of
    Nothing -> Right Nothing
    Just cArg ->
      case fromString cArg of
        Nothing -> Left $ "expected arg " <> argName <> " to be a natural non-zero number, instead got: " <> cArg
        Just n
          | n <= 0 -> Left $ "expected arg " <> argName <> " to be a natural non-zero number, instead got: " <> show n
          | otherwise -> Right $ Just n

parseFlagArg :: String -> Array String -> Boolean
parseFlagArg argName args =
  any (\a -> a == argName) args

maybe2 :: forall a b. (a -> b -> b) -> Maybe a -> b -> b
maybe2 fn maybeA b =
  maybe b (flip fn b) maybeA

parseAnnotateConfig :: Array String -> Either String AnnotateConfig
parseAnnotateConfig args = do
  verticalContext <- parseNaturalArg "-c" args
  pure defaultConfig
    <#> maybe2 withContextVertical verticalContext
    <#> withoutLinenumbers (parseFlagArg "-n" args)