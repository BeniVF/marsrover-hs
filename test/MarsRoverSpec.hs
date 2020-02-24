module MarsRoverSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import MarsRover
initPosition = (0, 0)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Rover should stay in the same place when" $ do
    it "there are no commands" $
      send (rover North (-1,1)) [] `shouldBe` rover North (-1,1)
    it "turning left and turning right commands are sent" $
      send (rover North (5,-7)) [TurnLeft, TurnRight] `shouldBe` rover North (5,-7)
    it "moving forward and backward commands are sent" $
      send (rover North (10,-1)) [MoveForward, MoveBackward] `shouldBe` rover North (10,-1)
    it "turning left, moving forward, moving backward and turning right commands are sent" $
      send (rover North initPosition) [TurnLeft, MoveForward, MoveBackward, TurnRight] `shouldBe` rover North initPosition
    it "moving 4 times to left" $
      send (rover North initPosition) [TurnLeft, TurnLeft, TurnLeft, TurnLeft] `shouldBe` rover North initPosition
    it "moving 4 times to right" $
      send (rover North initPosition) [TurnRight, TurnRight, TurnRight, TurnRight] `shouldBe` rover North initPosition
  describe "Rover should move forward when" $ do
    it "facing north" $
      send (rover North initPosition) [MoveForward] `shouldBe` rover North (0, 1)
    it "facing south" $
      send (rover South initPosition) [MoveForward] `shouldBe` rover South (0, -1)
    it "facing east" $
      send (rover East initPosition) [MoveForward] `shouldBe` rover East (1, 0)
    it "facing west" $
      send (rover West initPosition) [MoveForward] `shouldBe` rover West (-1, 0)
  describe "Rover should move backward when" $ do
    it "facing north" $
      send (rover North initPosition) [MoveBackward] `shouldBe` rover North (0, -1)
    it "facing south" $
      send (rover South initPosition) [MoveBackward] `shouldBe` rover South (0, 1)
    it "facing east" $
      send (rover East initPosition) [MoveBackward] `shouldBe` rover East (-1, 0)
    it "facing west" $ 
      send (rover West initPosition) [MoveBackward] `shouldBe` rover West (1, 0)
  describe "Rover should turn right when" $ do
    it "facing north" $
      send (rover North initPosition) [TurnRight] `shouldBe` rover East initPosition
    it "facing south" $
      send (rover South initPosition) [TurnRight] `shouldBe` rover West initPosition
    it "facing east" $
      send (rover East initPosition) [TurnRight] `shouldBe` rover South initPosition
    it "facing west" $
      send (rover West initPosition) [TurnRight] `shouldBe` rover North initPosition
  describe "Rover should turn left when" $ do
    it "facing north" $
      send (rover North initPosition) [TurnLeft] `shouldBe` rover West initPosition
    it "facing south" $
      send (rover South initPosition) [TurnLeft] `shouldBe` rover East initPosition
    it "facing east" $
      send (rover East initPosition) [TurnLeft] `shouldBe` rover North initPosition
    it "facing west" $
      send (rover West initPosition) [TurnLeft] `shouldBe` rover South initPosition
  describe "Rover should process multiple commands when" $ do
    it "moving forward multiple times" $
      send (rover North (1,2)) [MoveForward, MoveForward, MoveForward, MoveForward] `shouldBe` rover North (1, 6)
    it "moving backward multiple times" $
      send (rover West (-1, -1)) [MoveBackward, MoveBackward, MoveBackward, MoveBackward] `shouldBe` rover West (3, -1)
    it "turning left, moving forward, turning right and moving backward commands are sent" $
      send (rover East (-5,-5)) [TurnLeft, MoveForward, TurnRight, MoveBackward] `shouldBe` rover East (-6, -4)
    it "moving forward, turning right, moving backward and turning left commands are sent" $
      send (rover South (3,3)) [MoveForward, TurnRight, MoveBackward, TurnRight] `shouldBe` rover North (4, 2)