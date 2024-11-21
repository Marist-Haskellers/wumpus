module Types
  ( GameState (..),
    RoomState (..),
    PlayerState (..),
    Action (..),
    Result (..),
  )
where

data Room = Int

type Neighbors = [Room]

type Position = Room

data Sense = Hear | Smell | Feel

type Cave = [(Room, Neighbors)]

data Entity = Player | Wumpus | Bat | Pit

data Move = Left | Back | Right

data Action = Move Room | Shoot [Room] | Sense Room

data Result = Alive | WumpusKilled | EatenByWumpus | FellIntoPit | CaughtByBat

data GameState = GameState
  { playerPosition :: Position,
    wumpusPosition :: Position,
    batPosition :: Position,
    pitPosition :: Position,
    lastPosition :: Position
  }

data RoomState = RoomState
  { event :: Entity,
    sense :: Sense
  }

data PlayerState = PlaterState
  { arrowCount :: Int,
    status :: Bool
  }

-- Map Layout:
-- Position (1, Routes [2, 5, 8])
-- Position (2, Routes [1, 3, 10])
-- Position (3, Routes [2, 4, 12])
-- Position (4, Routes [3, 5, 14])
-- Position (5, Routes [1, 4, 6])
-- Position (6, Routes [5, 7, 15])
-- Position (7, Routes [6, 8, 17])
-- Position (8, Routes [1, 7, 9])
-- Position (9, Routes [8, 10, 18])
-- Position (10, Routes [2, 9, 11])
-- Position (11, Routes [10, 12, 19])
-- Position (12, Routes [3, 11, 13])
-- Position (13, Routes [12, 14, 20])
-- Position (14, Routes [4, 13, 15])
-- Position (15, Routes [6, 14, 16])
-- Position (16, Routes [15, 17, 20])
-- Position (17, Routes [7, 16, 18])
-- Position (18, Routes [9, 17, 19])
-- Position (19, Routes [11, 18, 20])
-- Position (20, Routes [13, 16, 19])