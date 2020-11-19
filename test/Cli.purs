module Test.Cli (main) where

import Cli (detectEncoding, parseAnnotateCliOptions)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..), fst)
import Debug.Trace (spy)
import Effect.Exception (Error)
import Node.Encoding (Encoding(..))
import Options.Applicative (ParserFailure(..), ParserResult(..), getParseResult, overFailure, renderFailure)
import Options.Applicative.Help (renderHelp)
import Prelude (class Monad, class Show, Unit, discard, show, (#), ($), (<<<))
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.String (shouldContain)

main :: forall a b. Monad b => MonadThrow Error a => SpecT a Unit b Unit
main = do
  detectEncodingTests
  parseAnnotateCliOptionsTests
      
detectEncodingTests :: forall a b. Monad b => MonadThrow Error a => SpecT a Unit b Unit
detectEncodingTests =
  describe "detectEncoding" do
    it "should correctly detect UTF8" do
      (show $ detectEncoding [0xEF, 0xBB, 0xBF]) `shouldEqual` show UTF8
    
    it "should correctly detect UTF16" do
      (show $ detectEncoding [0xFF, 0xFE]) `shouldEqual` show UTF16LE

    it "should default to UTF8 when it doesn't recognize the give data as BOM" do
      (show $ detectEncoding [0x51, 0x44, 0xAF]) `shouldEqual` show UTF8

parseAnnotateCliOptionsTests :: forall a b. Monad b => MonadThrow Error a => SpecT a Unit b Unit
parseAnnotateCliOptionsTests =
  describe "parseAnnotateCliOptions" do
    it "should report missing location option" do
      (parseErrorMsg $ parseAnnotateCliOptions []) `shouldContain` "Missing: (-l|--location LOCATION)"
    it "should not error on correct args" do
      (parseAnnotateCliOptions ["-l", "12:21"] # getParseResult # isJust) `shouldEqual` true

parseErrorMsg :: forall a. ParserResult a -> String
parseErrorMsg = show <<< case _ of
  Failure failure ->
    let 
      Tuple msg _ = renderFailure failure "test"
    in
      Just msg
  _ -> Nothing

