module Beacon
  (AnnotateConfig
  , defaultConfig
  , withLineNumbers
  , withContextAbove
  , withContextBelow
  , withContextLeft
  , withContextRight
  , CharacterLocation
  , characterLocation
  , annotate
  ) where

import Data.Array
import Prelude

import Data.FoldableWithIndex (foldrWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.String as String
import Data.String.CodeUnits (drop)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)

type AnnotateContext =
  { left :: Int
  , above :: Int
  , right :: Int
  , below :: Int
  }

newtype AnnotateConfig = AnnotateConfig
  { context :: AnnotateContext
  , decorated :: Boolean
  , lineNumbered :: Boolean
  }

defaultConfig :: AnnotateConfig
defaultConfig = AnnotateConfig
  { context :
    { above : 0
    , below : 0
    , left : 100
    , right : 100
    }
  , decorated : true
  , lineNumbered : true
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

newtype CharacterLocation = CharacterLocation
  { line :: Int
  , column :: Int
  }

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
addToColumn newCol (CharacterLocation r@{ column }) =
  CharacterLocation r { column = newCol + column }

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
  doTransformation i ln r@{ charLoc, contents } =
    if inContext { lineNo : i, before : above, after : below, pivot : line rec.charLoc } then
      r { contents = ln : contents }
    else
      r

columnContextTransformation :: AnnotateContext -> Transformation
columnContextTransformation { left, right } rec =
  foldr doTransformation cleanContentsRec rec.contents
  where
  cleanContentsRec = rec { contents = [] }
  doTransformation ln r@{ charLoc, contents } =
    r

buildTransformations :: AnnotateConfig -> Array Transformation
buildTransformations (AnnotateConfig { context, decorated, lineNumbered }) =
  [ Just $ columnContextTransformation context
  , if lineNumbered then Just lineNumberTransformation else Nothing
  , Just $ lineContextTranformation context
  , if decorated then Just decorateTransformation else Nothing
  ]
    # foldl removeNothings []
    # reverse
  where
  removeNothings acc = case _ of
    Just t -> t : acc
    Nothing -> acc

applyTransformation :: TransformationPayload -> Transformation -> TransformationPayload
applyTransformation payload t = t payload

annotate :: AnnotateConfig -> CharacterLocation -> String -> String
annotate config charLoc input = case lines input of
  [] -> ""
  contents -> joinWith "\n" decoratedContents.contents
    where
      decoratedContents =
        buildTransformations config
          # foldl applyTransformation { charLoc, contents }