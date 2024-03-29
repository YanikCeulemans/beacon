module Beacon
  (AnnotateConfig
  , defaultConfig
  , InputSrc(..)
  , withLineNumbers
  , withContextVertical
  , withContextHorizontal
  , withoutLinenumbers
  , CharacterLocation
  , characterLocation
  , annotate
  ) where

import Data.Array
import Prelude

import Control.Monad.Reader (Reader, ask, runReader)
import Control.MonadZero (guard)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Generic.Rep (class Generic)
import Data.String (joinWith)
import Data.String as String
import Data.String.Utils (lines)
import Data.Show.Generic (genericShow)

type AnnotateContext =
  { left :: Int
  , above :: Int
  , right :: Int
  , below :: Int
  }

data InputSrc
  = FilePath String
  | StdIn

derive instance genericInputSrc :: Generic InputSrc _
derive instance eqInputSrc :: Eq InputSrc
instance showInputSrc :: Show InputSrc where
  show = genericShow

newtype AnnotateConfig = AnnotateConfig
  { context :: AnnotateContext
  , decorated :: Boolean
  , lineNumbered :: Boolean
  , characterLocation :: CharacterLocation
  }

characterLocation' :: AnnotateConfig -> CharacterLocation
characterLocation' (AnnotateConfig { characterLocation }) = characterLocation

instance showAnnotateConfig :: Show AnnotateConfig where
  show _ = "annotate config show"

derive instance eqAnnotateConfig :: Eq AnnotateConfig

defaultConfig :: CharacterLocation -> AnnotateConfig
defaultConfig characterLocation = AnnotateConfig
  { context :
    { above : 0
    , below : 0
    , left : 100
    , right : 100
    }
  , decorated : true
  , lineNumbered : true
  , characterLocation
  }

withLineNumbers :: Boolean -> AnnotateConfig -> AnnotateConfig
withLineNumbers lineNumbered (AnnotateConfig annotateConfig) =
  AnnotateConfig $ annotateConfig { lineNumbered = lineNumbered }

withContextAbove :: Int -> AnnotateConfig -> AnnotateConfig
withContextAbove amount (AnnotateConfig annotateConfig) =
  AnnotateConfig $ annotateConfig { context { above = amount  } }

withContextBelow :: Int -> AnnotateConfig -> AnnotateConfig
withContextBelow amount (AnnotateConfig annotateConfig) =
  AnnotateConfig $ annotateConfig { context { below = amount  } }
  
withContextLeft :: Int -> AnnotateConfig -> AnnotateConfig
withContextLeft amount (AnnotateConfig annotateConfig) =
  AnnotateConfig $ annotateConfig { context { left = amount  } }

withContextRight :: Int -> AnnotateConfig -> AnnotateConfig
withContextRight amount (AnnotateConfig annotateConfig) =
  AnnotateConfig $ annotateConfig { context { right = amount  } }

withContextVertical :: Int -> AnnotateConfig -> AnnotateConfig
withContextVertical amount =
  withContextAbove amount >>> withContextBelow amount

withContextHorizontal :: Int -> AnnotateConfig -> AnnotateConfig
withContextHorizontal amount =
  withContextLeft amount >>> withContextRight amount

withoutLinenumbers :: Boolean -> AnnotateConfig -> AnnotateConfig
withoutLinenumbers notLineNumbered =
  withLineNumbers $ not notLineNumbered

newtype CharacterLocation = CharacterLocation
  { line :: Int
  , column :: Int
  }

derive instance eqCharacterLocation :: Eq CharacterLocation

instance showCharacterLocation :: Show CharacterLocation where
  show _ = "character location show"

type TransformationPayload =
  { charLoc :: CharacterLocation
  , contents :: Array String
  }

type Transformation = TransformationPayload -> TransformationPayload

-- | Create a character location using line number and column
characterLocation :: Int -> Int -> CharacterLocation
characterLocation line column = CharacterLocation { line, column }

line :: CharacterLocation -> Int
line (CharacterLocation { line }) = line - 1

column :: CharacterLocation -> Int
column (CharacterLocation { column }) = column - 1

addToColumn :: Int -> CharacterLocation -> CharacterLocation
addToColumn amount (CharacterLocation r@{ column }) =
  CharacterLocation r { column = column + amount }

subtractFromColumn :: Int -> CharacterLocation -> CharacterLocation
subtractFromColumn amount (CharacterLocation r@{ column }) =
  CharacterLocation r { column = column - amount }

updateLine :: Int -> CharacterLocation -> CharacterLocation
updateLine newLine (CharacterLocation r@{ line }) =
  CharacterLocation r { line = newLine }

subtractFromLine :: Int -> CharacterLocation -> CharacterLocation
subtractFromLine subtraction (CharacterLocation r@{ line }) =
  CharacterLocation r { line = line - subtraction }

arrowBodyLength :: Int
arrowBodyLength = 5

arrow :: Boolean -> Array String
arrow reversed =
  replicate (arrowBodyLength) "-"
    # rappend ["^"]
  where
    rappend =
      if reversed then append else flip append

decorate :: Int -> String
decorate amount =
  paddedStart
    # flip append (arrow $ safeAmount < arrowBodyLength)
    # joinWith ""
  where
    safeAmount = max 0 amount
    paddedStart =
      if safeAmount < arrowBodyLength then
        replicate safeAmount " "
      else
        replicate (safeAmount - arrowBodyLength) " "


lineNumberTransformation :: Transformation
lineNumberTransformation rec =
  foldrWithIndex buildTransformation cleanContentsRec rec.contents
  where
  cleanContentsRec = rec { contents = [] }
  buildTransformation i ln r@{ charLoc, contents } =
    if i == line charLoc then
      r { charLoc = addToColumn (String.length lineNumberPrefix) charLoc
        , contents = lineNumberedContents
        }
    else
      r { charLoc = charLoc, contents = lineNumberedContents }
    where
    lineNumberPrefix = show (i + 1) <> ": "
    lineNumberedContents = (lineNumberPrefix <> ln) : contents

decorateTransformation :: Transformation
decorateTransformation rec =
  foldrWithIndex buildTransformation cleanContentsRec rec.contents
  where
  cleanContentsRec = rec { contents = [] }
  decorateLine charLoc =
    decorate $ column charLoc
  buildTransformation i ln r@{ charLoc, contents } =
    if i == line charLoc then
      r { contents = ln : decorateLine charLoc : contents }
    else
      r { contents = ln : contents }

inContext :: { lineNo :: Int, before :: Int, after :: Int, pivot :: Int } -> Boolean
inContext { lineNo, before, after, pivot } =
  lineNo >= pivot - before && lineNo <= pivot + after

lineContextTranformation :: AnnotateContext -> Transformation
lineContextTranformation { above, below } rec =
  foldrWithIndex doTransformation cleanContentsRec rec.contents
    # _ { charLoc = updateLine (1 + above) rec.charLoc }
  where
  cleanContentsRec = rec { contents = [] }
  doTransformation i ln r@{ contents } =
    if inContext { lineNo : i, before : above, after : below, pivot : line rec.charLoc } then
      r { contents = ln : contents }
    else
      r

columnContextTransformation :: AnnotateContext -> Transformation
columnContextTransformation { left, right } rec =
  foldr doTransformation cleanContentsRec rec.contents
    # _ { charLoc = subtractFromColumn dropAmount rec.charLoc }
  where
  cleanContentsRec = rec { contents = [] }
  dropAmount = max 0 $ column rec.charLoc - left
  doTransformation ln r@{ charLoc, contents } =
    r
      { contents = 
        (String.drop dropAmount ln # String.take (column charLoc - dropAmount + right + 1)) : contents
      }

buildTransformations :: Reader AnnotateConfig (Array Transformation)
buildTransformations = do
  (AnnotateConfig { context, decorated, lineNumbered }) <- ask
  [ pure $ columnContextTransformation context
  , guard lineNumbered $> lineNumberTransformation
  , pure $ lineContextTranformation context
  , guard decorated $> decorateTransformation
  ]
    # catMaybes >>> pure

annotate :: AnnotateConfig -> String -> String
annotate cfg input = runReader (annotate' input) cfg

annotate' :: String -> Reader AnnotateConfig String
annotate' input = case lines input of
  [] -> pure ""
  contents -> do
    transformations <- buildTransformations
    charLoc <- characterLocation' <$> ask
    let
      foldStart = { charLoc, contents }
      resultPayload = foldl (#) foldStart transformations
    pure $ joinWith "\n" resultPayload.contents
