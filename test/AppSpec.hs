module AppSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import App

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "Rover should not able to process commands when" $ do
    it "empty string" $
      processCommands "" `shouldBe` Left "Invalid input: []"
    it "random value" $
      processCommands "hello world" `shouldBe` Left "Invalid input: [hello world]"
    it "bearing is invalid" $
      processCommands "X 1 1 LLL" `shouldBe` Left "Invalid bearing: [X]"
    it "one coordinate is a char" $
      processCommands "N F 1 LLL" `shouldBe` Left "Prelude.read: no parse"
    it "command is invalid" $
      processCommands "N 1 1 YLL" `shouldBe` Left "Invalid command: [Y]"
  describe "Rover should process command when" $ do
    it "there are no commands" $
      processCommands "S 1 1" `shouldBe` Right "S 1 1"
    it "there are commands" $
        processCommands "N 0 0 LFFFRFLF"  `shouldBe` Right "W -4 1"
