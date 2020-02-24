module Parser(
  parseInput
) where

import MarsRover
import Text.Read

parseInput:: String -> Either String (Rover, [Command])
parseInput = parseWords . words

parseWords:: [String] -> Either String (Rover, [Command])
parseWords (b : i : j : xs) = (,) <$> theRover <*> theCommands
  where
    theCoordinates = (,) <$> readEither i <*> readEither j
    theRover = rover <$> bearingForm b <*> theCoordinates
    theCommands = traverse commandFrom $ unwords xs
parseWords w = Left $ "Invalid input: [" ++ unwords w ++ "]"

commandFrom:: Char -> Either String Command
commandFrom 'L' = Right TurnLeft
commandFrom 'R' = Right TurnRight
commandFrom 'F' = Right MoveForward
commandFrom 'B' = Right MoveBackward
commandFrom x = Left $ "Invalid command: [" ++ [x] ++ "]"

bearingForm:: String -> Either String Bearing
bearingForm "N" = Right North
bearingForm "S" = Right South
bearingForm "W" = Right West
bearingForm "E" = Right East
bearingForm x = Left $ "Invalid bearing: [" ++ x ++ "]"

