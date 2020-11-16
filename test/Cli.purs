module Test.Cli (main) where

import Prelude

import Beacon (AnnotateConfig, defaultConfig, withContextAbove, withContextBelow, withoutLinenumbers)
import Cli (detectEncoding, parseAnnotateConfig)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (joinWith)
import Effect.Exception (Error)
import Node.Buffer (fromArray)
import Node.Encoding (Encoding(..))
import Test.Spec (SpecT(..), describe, it)
import Test.Spec.Assertions (shouldEqual)

main :: forall a b. Monad b => MonadThrow Error a => SpecT a Unit b Unit
main = do
  -- parseAnnotateConfigTests
  detectEncodingTests
      
-- parseAnnotateConfigTests :: forall a b. Monad b => MonadThrow Error a => SpecT a Unit b Unit
-- parseAnnotateConfigTests =
--   describe "parseAnnotateConfig" do
--     it "should parse -c context argument" do
--       parseAnnotateConfig ["-c", "2"] `shouldEqual` (Right (defaultConfig # withContextAbove 2 # withContextBelow 2))

--     it "should parse Nothing from empty arguments" do
--       parseAnnotateConfig [] `shouldEqual` (Right defaultConfig)

--     it "should parse -n context argument" do
--       parseAnnotateConfig ["-n"] `shouldEqual` (Right (defaultConfig # withoutLinenumbers true))

detectEncodingTests :: forall a b. Monad b => MonadThrow Error a => SpecT a Unit b Unit
detectEncodingTests =
  describe "detectEncoding" do
    it "should correctly detect UTF8" do
      (show $ detectEncoding [0xEF, 0xBB, 0xBF]) `shouldEqual` show UTF8
    
    it "should correctly detect UTF16" do
      (show $ detectEncoding [0xFF, 0xFE]) `shouldEqual` show UTF16LE

    it "should default to UTF8 when it doesn't recognize the give data as BOM" do
      (show $ detectEncoding [0x51, 0x44, 0xAF]) `shouldEqual` show UTF8