module Main where

import Game

exampleGame = Game (Location 1 1) [
  [Wall, Wall,  Wall,  Wall,  Wall],
  [Wall, Floor, Floor, Floor, Wall],
  [Wall, Floor, Floor, Floor, Wall],
  [Wall, Floor, Floor, Floor, Wall],
  [Wall, Floor, Floor, Floor, Wall],
  [Wall, Floor, Floor, Exit,  Wall],
  [Wall, Wall,  Wall,  Wall,  Wall]
  ]

main :: IO ()
main = putStrLn (showGame exampleGame)
