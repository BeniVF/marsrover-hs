
module MarsRover (
  send, 
  rover, 
  printRover,
  Rover,
  Bearing(..), 
  Command(..)
)  where

import Data.Char

data Rover = Rover Bearing Coordinates deriving (Show, Eq)
newtype Coordinates = Coordinates (Int, Int) deriving (Show, Eq)
data Command = MoveForward | MoveBackward | TurnRight | TurnLeft deriving (Show, Eq)
data Bearing = North | South | West | East deriving (Show, Eq)

instance Semigroup Coordinates where
  Coordinates (x1, y1) <> Coordinates (x2, y2) = Coordinates (x1+x2, y1+y2)

rover :: Bearing -> (Int,Int) -> Rover
rover bearing coordinates = Rover bearing (Coordinates coordinates)

send :: Rover -> [Command] -> Rover
send = foldl $ flip executeCommand

executeCommand :: Command -> Rover -> Rover
executeCommand MoveForward = move (\x b -> x <> forward b)
executeCommand MoveBackward = move (\x b -> x <> forward (opposite b))
executeCommand TurnRight = turn right
executeCommand TurnLeft = turn (opposite . right)

move :: (Coordinates -> Bearing -> Coordinates) -> Rover -> Rover
move f (Rover b c) = Rover b $ f c b

turn :: (Bearing -> Bearing) -> Rover -> Rover
turn f (Rover b c) = Rover (f b) c

forward :: Bearing -> Coordinates
forward North = Coordinates (0, 1)
forward South = Coordinates (0, -1)
forward East = Coordinates (1, 0)
forward West = Coordinates (-1, 0)

right:: Bearing -> Bearing
right North = East
right South = West
right West = North
right East = South

opposite :: Bearing -> Bearing
opposite North = South
opposite South = North
opposite West = East
opposite East = West

printRover:: Rover -> String
printRover (Rover b (Coordinates (i,j))) = printBearing b ++ " " ++ show i ++ " " ++ show j

printBearing:: Bearing -> String
printBearing North = "N"
printBearing West = "W"
printBearing East = "E"
printBearing South = "S"