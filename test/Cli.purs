module Test.Cli (main) where

import Prelude

import Beacon (InputSrc(..), characterLocation, defaultConfig, withContextHorizontal, withContextVertical, withoutLinenumbers)
import Cli (detectEncoding, parseAnnotateCliOptions)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array ((:))
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Debug.Trace (spy)
import Effect.Exception (Error)
import Node.Encoding (Encoding(..))
import Options.Applicative (ParserFailure(..), ParserResult(..), getParseResult, overFailure, renderFailure)
import Options.Applicative.Help (renderHelp)
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
      let
        actual =
          parseAnnotateCliOptions []
            # parseErrorMsg
      actual `shouldContain` "Missing: (-l|--location LOCATION)"

    -- it "should work" do
    --   let
    --     charLocSpec charLoc =
    --       (parseAnnotateCliOptions ("-l" : [charLoc]) # parseErrorMsg)
    --         `shouldContain` ("test")
    --   traverse charLocSpec ["a", "a:a", "1:a", "a:1", "a,a"]

    it "should report incorrect location option" do
      let
        actual =
          parseAnnotateCliOptions ["-l", "a:a"]
            # parseErrorMsg
      actual `shouldContain` "Expected a value to be supplied in the form 'n:n'"

    it "should parse character location" do
      let
        actual =
          parseAnnotateCliOptions ["-l", "12:21"]
            # getParseResult
            <#> _.annotateConfig
        expected =
          characterLocation 12 21 # defaultConfig # Just
      actual `shouldEqual` expected

    it "should parse context" do
      let
        actual = 
          parseAnnotateCliOptions ["-l", "12:21", "-c", "2"]
            # getParseResult
            <#> _.annotateConfig
        expected =
          characterLocation 12 21 # defaultConfig # withContextVertical 2 # Just
      actual `shouldEqual` expected

    it "should identify InputSrc stdin without FILEPATH argument" do
      let
        actual =
          parseAnnotateCliOptions ["-l", "12:21"]
            # getParseResult
            <#> _.inputSrc
        expected = Just StdIn
      actual `shouldEqual` expected

    it "should parse FILEPATH argument" do
      let
        actual =
          parseAnnotateCliOptions ["-l", "12:21", "filepath"]
            # getParseResult
            <#> _.inputSrc
        expected =
          Just $ FilePath "filepath"
      actual `shouldEqual` expected

    it "should parse no-line-numbers flag" do
      let
        actual =
          parseAnnotateCliOptions ["-l", "12:21", "-n"]
            # getParseResult
            <#> _.annotateConfig
        expected =
          characterLocation 12 21
            # defaultConfig
            # withoutLinenumbers true
            # Just
      actual `shouldEqual` expected
    
    it "should parse horizontal context" do
      let
        actual =
          parseAnnotateCliOptions ["-l", "12:21", "--horizontal-context", "42"]
            # getParseResult
            <#> _.annotateConfig
        expected =
          characterLocation 12 21
            # defaultConfig
            # withContextHorizontal 42
            # Just
      actual `shouldEqual` expected

parseErrorMsg :: forall a. ParserResult a -> String
parseErrorMsg = show <<< case _ of
  Failure failure ->
    let 
      Tuple msg _ = renderFailure failure "test"
    in
      Just msg
  _ -> Nothing

