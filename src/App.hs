module App(processCommands) where

import Parser
import MarsRover

processCommands :: String -> Either String String
processCommands = fmap(printRover . uncurry send) .parseInput
