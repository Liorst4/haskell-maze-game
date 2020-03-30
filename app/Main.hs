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
main = do
  putStrLn "You are \"P\""
  putStrLn "You need to get to \"*\""
  putStrLn "Move by typing \"Up\", \"Down\", \"Left\" or \"Right\""
  gameLoop exampleGame

gameLoop :: Game -> IO ()
gameLoop game = do
  putStrLn (showGame game)
  if over game
    then do putStrLn "You won!"
            return ()
    else do
    putStrLn "Enter your next move:"
    userInput <- getLine
    putStrLn userInput
    let nextGame =
            case moveFromString userInput of
            Nothing -> game
            Just move -> case turn game move of
                Nothing -> game
                Just nextTurnGame -> nextTurnGame
    gameLoop nextGame
