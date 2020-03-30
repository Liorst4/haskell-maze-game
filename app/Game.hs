module Game where

import Data.List

-- TODO: Derive from Show
data Tile = Floor
          | Wall
          | Exit
    deriving (Eq)

data Move = Left
          | Right
          | Up
          | Down

data Location = Location {x :: Int, y :: Int}
  deriving (Eq)

-- TODO: Derive from Show
data Game = Game {
  player :: Location,
  board :: [[Tile]]
}

over :: Game -> Bool
over game = case playerTile of
  Just t -> t == Exit
  Nothing -> True
  where playerTile = tileAt (board game) (player game)

nextLocation :: Location -> Move -> Location
nextLocation player move =
  case move of
    Game.Left -> Location (x player - 1) (y player)
    Game.Right -> Location (x player + 1) (y player)
    Game.Up -> Location (x player) (y player - 1)
    Game.Down -> Location (x player) (y player + 1)

tileAt :: [[Tile]] -> Location -> Maybe Tile
tileAt board location =
  if y location > length board || x location > length row
  then Nothing
  else Just (row !! x location)
  where row = board !! y location

turn :: Game -> Move -> Maybe Game
turn game move =
  case tileAt (board game) nextPlayerLocation of
    Just tile -> case tile of
      Wall -> Nothing
      _ -> Just (Game nextPlayerLocation (board game))
    Nothing -> Nothing
  where nextPlayerLocation = nextLocation (player game) move

-- TODO: 1. Generate the board purly
--       2. Edit the rendered board to add the player
showGame :: Game -> String
showGame game = intercalate "\n" (map (showRows game) [0..length (board game) - 1])
  where
    showRows game index = [showLocation game (Location a b) | a <- [0..length ((board game) !! index) - 1], b <- [index]]
    showLocation game location =
        if location == player game
        then playerChar
        else (case tileAt (board game) location of
                Nothing -> '?'
                Just t -> case t of
                            Floor -> ' '
                            Wall -> 'O'
                            Exit -> '*')
    playerChar = 'P'
