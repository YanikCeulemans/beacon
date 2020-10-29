module Main where

import Prelude

import Beacon (annotate, characterLocation, defaultConfig)
import Data.Array (length, take)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.String.Utils (lines)
import Debug.Trace (spy)
import Effect (Effect, whileE)
import Effect.Aff (Aff, effectCanceler, joinFiber, launchAff, launchAff_, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Effect.Ref as Ref
import Node.Buffer (toArray)
import Node.Encoding (Encoding(..))
import Node.Process (argv, stderr, stdin, stdoutIsTTY)
import Node.Stream (Readable, destroy, onClose, onData, onDataString, onEnd, onError, onFinish, onReadable, pause, readString, resume, setEncoding)

foreign import stdinIsTTY :: Boolean

readText :: forall w. Readable w -> Aff (Maybe String)
readText r = makeAff $ \res ->
  if stdinIsTTY then do
    res $ Right Nothing
    pure nonCanceler
  else do
    dataRef <- Ref.new ""
    -- We cannot assume UTF8 -> We need to do BOM sniffing to determine encoding. See:
    -- https://en.wikipedia.org/wiki/Byte_order_mark
    onDataString r UTF8 \chunk ->
      Ref.modify_ (_ <> chunk) dataRef
    onEnd r do
      allData <- Ref.read dataRef
      res $ Right (Just allData)
    onError r $ Left >>> res
    pure $ effectCanceler (pause r)

main :: Effect Unit
main = do
  onData stdin \buff -> do
    arr <- toArray buff <#> take 5
    logShow arr
  launchAff_ do
    t <- readText stdin
    -- let _ = spy "t" t
    liftEffect $ logShow $ t <#> lines <#> length
    cmdLineArgs <- liftEffect argv
    liftEffect $ logShow cmdLineArgs
