module Main where

import App

main :: IO ()
main = interact $ either id id . processCommands
