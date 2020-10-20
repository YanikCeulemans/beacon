module Test.Main where

import Prelude

import Beacon (annotate, characterLocation, defaultConfig, withContextAbove, withLineNumbers)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  launchAff_ $ runSpec [consoleReporter] do
    describe "annotate" do
      it "should correctly annotate a single line with defaultConfig" do
        annotate defaultConfig (characterLocation 1 1) "line 1" 
          `shouldEqual` "1: line 1\n   ^-----"
      
      it "should correctly flip the annotate arrow with defaultConfig" do
        annotate defaultConfig (characterLocation 1 6) "line 1" 
          `shouldEqual` "1: line 1\n   -----^"

      it "should correctly annotate a single line without line numbers" do
        let cfg = defaultConfig # withLineNumbers false
        annotate cfg (characterLocation 1 3) "line 1" 
          `shouldEqual` "line 1\n  ^-----"
            
      it "should correctly annotate multiple lines with defaultConfig" do
        annotate defaultConfig (characterLocation 2 2) "line 1\nline 2\nline 3"
          `shouldEqual` "2: line 2\n    ^-----"

      it "should correctly annotate the first line multiple lines with defaultConfig" do
        annotate defaultConfig (characterLocation 1 2) "line 1\nline 2\nline 3"
          `shouldEqual` "1: line 1\n    ^-----"
      
      it "should correctly annotate the first line for multiple lines with defaultConfig" do
        annotate defaultConfig (characterLocation 1 2) "line 1\nline 2\nline 3"
          `shouldEqual` "1: line 1\n    ^-----"

      it "should correctly annotate multiple lines with context above" do
        let cfg = defaultConfig # withContextAbove 1
        annotate cfg (characterLocation 2 2) "line 1\nline 2\nline3"
          `shouldEqual` "1: line 1\n2: line 2\n    ^-----"
