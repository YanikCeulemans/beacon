module Cli
  ( annotateInput
  , detectEncoding
  , class ArgsProvider
  , provideArgs
  , parseAnnotateCliOptions
  , AnnotateCliOptions
  ) where

import Options.Applicative (ParseError(..), Parser, ParserResult, abortOption, argument, defaultPrefs, eitherReader, execParserPure, fullDesc, help, helper, info, int, long, metavar, option, progDesc, short, showDefault, str, switch, value, (<**>))
import Prelude

import Beacon (AnnotateConfig, CharacterLocation, InputSrc(..), characterLocation, defaultConfig, withContextHorizontal, withContextVertical, withoutLinenumbers)
import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), withExceptT)
import Control.MonadZero (guard)
import Data.Array (drop, slice, snoc)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, makeAff, message, nonCanceler, try)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Node.Buffer (Buffer, concat, toArray, toString)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile)
import Node.Path (relative)
import Node.Process (argv, cwd, stdin)
import Node.Stream (Readable, onData, onEnd, onError, pause)

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

horizontalContextParser :: Parser Int
horizontalContextParser =
  option int
    ( long "horizontal-context"
    <> metavar "AMOUNT"
    <> showDefault
    <> value 100
    <> help "The amount of context to show before and after the location"
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

annotateConfigParser :: Parser { annotateConfig :: AnnotateConfig, inputSrc :: InputSrc }
annotateConfigParser = ado
  characterLocation <- characterLocationParser
  contextAmount <- contextParser
  horizontalContextAmount <- horizontalContextParser
  disableLineNumbers <- disableLineNumbersParser
  src <- inputSrcParser
  in
    { annotateConfig :
      defaultConfig characterLocation
        # withContextVertical contextAmount
        # withContextHorizontal horizontalContextAmount
        # withoutLinenumbers disableLineNumbers
    , inputSrc : src
    }

annotateInput :: InputSrc -> ExceptT String Aff String
annotateInput src =
  case src of
    FilePath filePath ->
      fromFile filePath
    StdIn -> 
      fromStdin
  where
  fromBuffer :: Buffer -> Aff String
  fromBuffer buff = liftEffect do
    encoding <- detectEncoding <$> toArray buff
    toString encoding buff
  fromStdin :: ExceptT String Aff String
  fromStdin = ExceptT do
    maybeBuff <- readFromStream stdin
    case maybeBuff of
      Nothing ->
        pure $ Left "No stdin stream found as input, see --help for more info"
      Just buff ->
        fromBuffer buff <#> Right
  fromFile :: String -> ExceptT String Aff String
  fromFile relPath = withExceptT message <<< ExceptT $ try do
    currPath <- liftEffect cwd
    let absPath = relative currPath relPath
    fileBuff <- readFile absPath
    fromBuffer fileBuff

ensureNatural :: Array (Maybe Int) -> Array (Maybe Int)
ensureNatural maybeXs = do
  maybeX <- maybeXs
  pure do
    x <- maybeX
    guard $ x > 0
    pure x

parseCharacterLocation :: String -> Either String CharacterLocation
parseCharacterLocation s = 
  case split (Pattern ":") s <#> Int.fromString # ensureNatural of
    [Just line, Just column] ->
      Right $ characterLocation line column
    _ ->
      Left $ "Expected a value to be supplied in the form 'n:n' "
        <> "where n is any natural number. Instead got '" <> s <> "'"

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

versionParser :: forall a. Parser (a -> a)
versionParser = abortOption (InfoMsg "beacon v1.0.0")
  ( long "version"
  <> short 'v'
  <> help "Show the current beacon version"
  )

type AnnotateCliOptions =
  { annotateConfig :: AnnotateConfig
  , inputSrc :: InputSrc
  }

parseAnnotateCliOptions :: Array String -> ParserResult AnnotateCliOptions
parseAnnotateCliOptions =
  execParserPure defaultPrefs annotateCliOptionsParser
  where
  annotateCliOptionsParser =
    info (annotateConfigParser <**> versionParser <**> helper)
      ( fullDesc
      <> progDesc "Show line and column number given input and location"
      )

class ArgsProvider m where
  provideArgs :: m (Array String)

instance argvArgsProvider :: ArgsProvider Effect where
  provideArgs = argv <#> drop 2
