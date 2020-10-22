module Test.Main where

import Prelude
import Test.Cli

import Beacon (annotate, characterLocation, defaultConfig, withContextAbove, withContextBelow, withContextLeft, withContextRight, withLineNumbers)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Test.Cli as CliTest
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  launchAff_ $ runSpec [consoleReporter] do
    CliTest.main
    describe "annotate" do
      it "should annotate a single line with defaultConfig" do
        annotate defaultConfig (characterLocation 1 1) "line 1" 
          `shouldEqual` "1: line 1\n   ^-----"
      
      it "should flip the annotate arrow with defaultConfig" do
        annotate defaultConfig (characterLocation 1 6) "line 1" 
          `shouldEqual` "1: line 1\n   -----^"

      it "should annotate a single line without line numbers" do
        let cfg = defaultConfig # withLineNumbers false
        annotate cfg (characterLocation 1 3) "line 1" 
          `shouldEqual` "line 1\n  ^-----"
            
      it "should annotate multiple lines with defaultConfig" do
        annotate defaultConfig (characterLocation 2 2) "line 1\nline 2\nline 3"
          `shouldEqual` "2: line 2\n    ^-----"

      it "should annotate the first line multiple lines with defaultConfig" do
        annotate defaultConfig (characterLocation 1 2) "line 1\nline 2\nline 3"
          `shouldEqual` "1: line 1\n    ^-----"
      
      it "should annotate the first line for multiple lines with defaultConfig" do
        annotate defaultConfig (characterLocation 1 2) "line 1\nline 2\nline 3"
          `shouldEqual` "1: line 1\n    ^-----"

      it "should annotate multiple lines with context above" do
        let cfg = defaultConfig # withContextAbove 1
        annotate cfg (characterLocation 2 2) "line 1\nline 2\nline 3"
          `shouldEqual` "1: line 1\n2: line 2\n    ^-----"

      it "should annotate multiple lines with context below" do
        let cfg = defaultConfig # withContextBelow 1
        annotate cfg (characterLocation 2 2) "line 1\nline 2\nline 3"
          `shouldEqual` "2: line 2\n    ^-----\n3: line 3"

      it "should annotate multiple lines with context above and below" do
        let cfg = defaultConfig # withContextAbove 1 # withContextBelow 1
        annotate cfg (characterLocation 2 2) "line 1\nline 2\nline 3"
          `shouldEqual` "1: line 1\n2: line 2\n    ^-----\n3: line 3"

      it "should annotate multiple lines with context left" do
        let cfg = defaultConfig # withContextLeft 2
        annotate cfg (characterLocation 2 4) "line 1\nline 2\nline 3"
          `shouldEqual` "2: ine 2\n-----^"

      it "should annotate multiple lines with context right" do
        let cfg = defaultConfig # withContextRight 2
        annotate cfg (characterLocation 2 4) "line 1\nline 2 with additional content\nline 3"
          `shouldEqual` "2: line 2\n -----^"

      it "should annotate multiple lines with context right and left" do
        let cfg = defaultConfig # withContextLeft 2 # withContextRight 2
        annotate cfg (characterLocation 2 4) "line 1\nline 2 with additional content\nline 3"
          `shouldEqual` "2: ine 2\n-----^"

      it "should annotate multiple lines with context right, left, above and below" do
        let cfg = defaultConfig # withContextLeft 2 # withContextRight 2 # withContextAbove 1 # withContextBelow 1
        annotate cfg (characterLocation 2 4) "line 1\nline 2 with additional content\nline 3"
          `shouldEqual` "1: ine 1\n2: ine 2\n-----^\n3: ine 3"
