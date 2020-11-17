module Cli where

import Options.Applicative
import Prelude

import Beacon (AnnotateConfig, CharacterLocation, InputSrc(..), characterLocation, defaultConfig, inputSrc, withContextAbove, withContextBelow, withContextVertical, withoutLinenumbers)
import Control.Alt ((<|>))
import Data.Array (any, dropWhile, last, slice, snoc, take)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), split)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, makeAff, message, nonCanceler, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Node.Buffer (Buffer, concat, toArray, toString)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (exists, readFile)
import Node.Path (relative)
import Node.Process (cwd, stdin)
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

parseCharacterLocation :: String -> Either String CharacterLocation
parseCharacterLocation s = 
  case split (Pattern ":") s <#> fromString of
    [Just line, Just column] ->
      Right $ characterLocation line column
    _ ->
      Left $ "Expected a value to be supplied in the form 'n:n' "
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


contextParser :: Parser Int
contextParser =
  option int
    ( long "context"
    <> short 'c'
    <> metavar "AMOUNT"
    <> showDefault
    <> value 0
    <> help "The amount of context to show above and below the location"
    )

disableLineNumbersParser :: Parser Boolean
disableLineNumbersParser =
  switch
    ( long "no-line-numbers"
    <> short 'n'
    <> help "Whether or not to disable showing line numbers in the output"
    )

inputSrcParser :: Parser InputSrc
inputSrcParser =
  fileInputParser <|> pure StdIn
  where
  fileInputParser =
    argument (str <#> FilePath)
      ( metavar "FILEPATH"
      <> help "The relative path of the input file, uses stdin if not given"
      )
    

annotateConfigParser :: Parser AnnotateConfig
annotateConfigParser = ado
  characterLocation <- characterLocationParser
  contextAmount <- contextParser
  disableLineNumbers <- disableLineNumbersParser
  src <- inputSrcParser
  in defaultConfig src characterLocation
    # withContextVertical contextAmount
    # withoutLinenumbers disableLineNumbers

parseAnnotateConfig :: Effect AnnotateConfig
parseAnnotateConfig =
  execParser opts
  where
    opts = info (annotateConfigParser <**> helper)
      ( fullDesc
      <> progDesc "Show line and column number given input and location"
      )

annotateInput :: AnnotateConfig -> Aff (Either String String)
annotateInput annotateConfig =
  case inputSrc annotateConfig of
    FilePath filePath ->
      fromFile filePath
    StdIn -> 
      fromStdin
  where
  fromBuffer :: Buffer -> Aff String
  fromBuffer buff = liftEffect do
    encoding <- detectEncoding <$> toArray buff
    toString encoding buff
  fromStdin :: Aff (Either String String)
  fromStdin = do
    maybeBuff <- readFromStream stdin
    case maybeBuff of
      Nothing ->
        pure $ Left "No stdin stream found as input, see --help for more info"
      Just buff ->
        fromBuffer buff <#> Right
  fromFile :: String -> Aff (Either String String)
  fromFile relPath = lmap message <$> try do
    currPath <- liftEffect cwd
    let absPath = relative currPath relPath
    fileBuff <- readFile absPath
    fromBuffer fileBuff

characterLocationParser :: Parser CharacterLocation
characterLocationParser = 
  option characterLocationReader
    ( long "location"
    <> short 'l'
    <> metavar "LOCATION"
    <> help "The location to show, in the format 'line:column' e.g. '15:42'"
    )
  where
  characterLocationReader = eitherReader parseCharacterLocation

