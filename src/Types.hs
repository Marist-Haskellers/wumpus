module Types where

type Position = Int

-- if move is an enum then it forces the game to have
--    # of enums amount of connections per cave
--    a more generic way of representing moves could be
--    used but it may result it move names which are bland (e.i. to room 3)
--    or move names which are incorrect (e.i. moving left when in that postion you can only move right)
-- For a decahedron it Left Right Back make sense for every move as you will always have those options
--    if orientated correctly

data GameState = GameState {
    wumpus :: WumpusState,
    environment :: EnvironmentState,
    player :: PlayerState
}

data PlayerState = PlayerState {
  playerPosition :: Position,
  lastPosition :: Position,
  playerArrowCount :: Int
} deriving Show

data WumpusState = WumpusState {
  wumpusPosition :: Position
} deriving Show

data EnvironmentState = EnvironmentState
  { hazards :: [(Position, Hazard)]
  }

data Hazard = Bats | Pit deriving (Eq, Show)

type CaveLayout = [(Position, [Position])]

-- Map Layout:
decahedron :: CaveLayout
decahedron =
  [ (1, [2, 8, 5]),
    (2, [3, 10, 1]),
    (3, [4, 12, 2]),
    (4, [5, 14, 3]),
    (5, [1, 6, 4]),
    (6, [5, 7, 15]),
    (7, [6, 8, 17]),
    (8, [1, 9, 7]),
    (9, [8, 10, 18]),
    (10, [9, 2, 11]),
    (11, [10, 12, 19]),
    (12, [11, 3, 13]),
    (13, [12, 14, 20]),
    (14, [13, 4, 15]),
    (15, [6, 16, 14]),
    (16, [15, 17, 20]),
    (17, [16, 7, 18]),
    (18, [17, 9, 19]),
    (19, [18, 11, 20]),
    (20, [13, 16, 19])
  ]


