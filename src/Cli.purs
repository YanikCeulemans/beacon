module Cli where

import Prelude

import Beacon (AnnotateConfig, CharacterLocation, characterLocation, defaultConfig, withContextVertical, withoutLinenumbers)
import Data.Array (any, dropWhile, last, slice, snoc, take)
import Data.Either (Either(..), note)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), split)
import Effect.Aff (Aff, effectCanceler, makeAff, nonCanceler)
import Effect.Ref as Ref
import Node.Buffer (Buffer, concat)
import Node.Encoding (Encoding(..))
import Node.Stream (Readable, onData, onEnd, onError, pause)

parseNaturalArg :: String -> Array String -> Either String (Maybe Int)
parseNaturalArg argName args =
  case dropWhile (\a -> a /= argName) args # take 2 # last of
    Nothing -> Right Nothing
    Just naturalArg ->
      case fromString naturalArg of
        Nothing -> Left $ "expected arg " <> argName <> " to be a natural non-zero number, instead got: " <> naturalArg
        Just n
          | n <= 0 -> Left $ "expected arg " <> argName <> " to be a natural non-zero number, instead got: " <> show n
          | otherwise -> Right $ Just n

parseFlagArg :: String -> Array String -> Boolean
parseFlagArg argName args =
  any (\a -> a == argName) args

parseStrArg :: String -> Array String -> Maybe String
parseStrArg argName args =
  dropWhile (\a -> a /= argName) args # take 2 # last

maybe2 :: forall a b. (a -> b -> b) -> Maybe a -> b -> b
maybe2 fn maybeA b =
  maybe b (flip fn b) maybeA

parseAnnotateConfig :: Array String -> Either String AnnotateConfig
parseAnnotateConfig args = do
  verticalContext <- parseNaturalArg "-c" args
  pure defaultConfig
    <#> maybe2 withContextVertical verticalContext
    <#> withoutLinenumbers (parseFlagArg "-n" args)

parseCharacterLocation :: Array String -> Either String CharacterLocation
parseCharacterLocation args = do
  charLocStr <- note "Required arg -l not found." $ parseStrArg "-l" args
  fromString' charLocStr
  where
    fromString' s =
      case split (Pattern ":") s <#> fromString of
        [Just line, Just column] ->
          Right $ characterLocation line column
        _ ->
          Left $ "Expected -l arg to be supplied a value in the form 'n:n' "
            <> "where n is any natural number. Instead got '" <> s <> "'"

detectEncoding :: Array Int -> Encoding
detectEncoding arr = case slice 0 2 arr of
  [0xFF, 0xFE] -> UTF16LE
  _ -> UTF8

foreign import stdinIsTTY :: Boolean

readFromStream :: forall w. Readable w -> Aff (Maybe Buffer)
readFromStream r = makeAff $ \res ->
  if stdinIsTTY then do
    res $ Right Nothing
    pure nonCanceler
  else do
    dataRef <- Ref.new []
    onData r \buffChunk -> do
      Ref.modify_ (flip snoc buffChunk) dataRef
    onEnd r do
      allData <- concat =<< Ref.read dataRef
      res $ Right (Just allData)
    onError r $ Left >>> res
    pure $ effectCanceler (pause r)