module Test.Cli (main) where

import Prelude

import Beacon (AnnotateConfig, defaultConfig, withContextAbove, withContextBelow, withoutLinenumbers)
import Cli (detectEncoding, parseAnnotateConfig)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Exception (Error)
import Node.Buffer (fromArray)
import Node.Encoding (Encoding(..))
import Test.Spec (SpecT(..), describe, it)
import Test.Spec.Assertions (shouldEqual)

main :: forall t3 t4. Monad t4 => MonadThrow Error t3 => SpecT t3 Unit t4 Unit
main = do
  parseAnnotateConfigTests
  detectEncodingTests
      
parseAnnotateConfigTests =
  describe "parseAnnotateConfig" do
    it "should parse -c context argument" do
      parseAnnotateConfig ["-c", "2"] `shouldEqual` (Right (defaultConfig # withContextAbove 2 # withContextBelow 2))

    it "should parse Nothing from empty arguments" do
      parseAnnotateConfig [] `shouldEqual` (Right defaultConfig)

    it "should parse -n context argument" do
      parseAnnotateConfig ["-n"] `shouldEqual` (Right (defaultConfig # withoutLinenumbers true))

detectEncodingTests =
  describe "detectEncoding" do
    it "should correctly detect UTF8" do
      detectEncoding [0xEF, 0xBB, 0xBF] `shouldEqual` UTF8