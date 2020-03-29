module Game where

data Tile = Floor
          | Wall
          | Exit
    deriving (Eq)

data Move = Left
          | Right
          | Up
          | Down

data Location = Location {x :: Int, y :: Int}

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
      Floor -> Just (Game nextPlayerLocation (board game))
      _ -> Nothing
    Nothing -> Nothing
  where nextPlayerLocation = nextLocation (player game) move
