{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

{-# HLINT ignore "Use forM_" #-}

module Types (module Types) where

import System.Random (StdGen)

type Position = Int

data GameStatus = Ongoing | GameOver String deriving (Show, Eq)

-- if move is an enum then it forces the game to have
--    # of enums amount of connections per cave
--    a more generic way of representing moves could be
--    used but it may result it move names which are bland (e.i. to room 3)
--    or move names which are incorrect (e.i. moving left when in that position you can only move right)
-- For a decahedron it Left Right Back make sense for every move as you will always have those options
--    if orientated correctly
data Move = MoveLeft | MoveRight | MoveBack deriving (Show, Eq)

data Action = MoveAction Move | ShootAction [Position] | SenseAction Sense deriving (Show, Eq)

data GameState = GameState
  { playerState :: PlayerState,
    wumpusState :: WumpusState,
    environmentState :: EnvironmentState,
    gen :: StdGen,
    gameStatus :: GameStatus
  }
  deriving (Show, Eq)

-- will have to be set to the correct position on game start to orientate player
data PlayerState = Player
  { playerPosition :: Position,
    lastPosition :: Position,
    playerArrowCount :: Int,
    playerHasShot :: Bool
  }
  deriving (Show, Eq)

data WumpusState = WumpusState
  { wumpusPosition :: Position
  }
  deriving (Show, Eq)

data EnvironmentState = EnvironmentState
  { hazards :: [(Position, Hazard)]
  }
  deriving (Show, Eq)

data Hazard = Bats | Pit deriving (Show, Eq)

type CaveLayout = [(Position, [Position])]

type MoveLayout = [((Position, Position), [Position])]

data Sense = SmellWumpus | HearBats | FeelDraft deriving (Show, Eq)

-- Generate sense data for the entire cave layout
generateSenseData :: CaveLayout -> GameState -> [(Position, [Sense])]
generateSenseData layout gameState =
  map (\(pos, neighbors) -> (pos, sensesForCave pos neighbors)) layout
  where
    sensesForCave :: Position -> [Position] -> [Sense]
    sensesForCave pos neighbors =
      let wumpusPos = wumpusPosition $ wumpusState gameState
          hazardMap = hazards $ environmentState gameState
          hazardAt n = lookup n hazardMap
       in concatMap (senseForNeighbor pos) neighbors ++ wumpusSense pos neighbors wumpusPos

    senseForNeighbor :: Position -> Position -> [Sense]
    senseForNeighbor current neighbor =
      case lookup neighbor (hazards $ environmentState gameState) of
        Just Bats -> [HearBats]
        Just Pit -> [FeelDraft]
        Nothing -> []

    wumpusSense :: Position -> [Position] -> Position -> [Sense]
    wumpusSense current neighbors wumpusPos
      | wumpusPos == current = [] -- Wumpus is in the same cave; player will see it
      | wumpusPos `elem` neighbors = [SmellWumpus]
      | otherwise = []

displaySenses :: Position -> [(Position, [Sense])] -> IO ()
displaySenses current senses =
  case lookup current senses of
    Just senseList -> mapM_ (putStrLn . describeSense) senseList
    Nothing -> return ()

describeSense :: Sense -> String
describeSense SmellWumpus = "You smell something foul nearby."
describeSense HearBats = "You hear the flapping of wings."
describeSense FeelDraft = "You feel a draft nearby."

-- Static map for the cave
decahedron :: CaveLayout
decahedron =
  [ (1, [2, 5, 8]),
    (2, [1, 3, 10]),
    (3, [2, 4, 12]),
    (4, [3, 5, 14]),
    (5, [1, 4, 6]),
    (6, [5, 7, 15]),
    (7, [6, 8, 17]),
    (8, [1, 7, 9]),
    (9, [8, 10, 18]),
    (10, [2, 9, 11]),
    (11, [10, 12, 19]),
    (12, [3, 11, 13]),
    (13, [12, 14, 20]),
    (14, [4, 13, 15]),
    (15, [6, 14, 16]),
    (16, [15, 17, 20]),
    (17, [7, 16, 18]),
    (18, [9, 17, 19]),
    (19, [11, 18, 20]),
    (20, [13, 16, 19])
  ]

-- Cave mapping for the move function
caveMap :: MoveLayout
caveMap =
  [ ((1, 2), [8, 5]),
    ((1, 5), [2, 8]),
    ((1, 8), [5, 2]),
    ((2, 1), [3, 10]),
    ((2, 3), [10, 1]),
    ((2, 10), [1, 3]),
    ((3, 2), [4, 12]),
    ((3, 4), [12, 2]),
    ((3, 12), [2, 4]),
    ((4, 3), [5, 14]),
    ((4, 5), [14, 3]),
    ((4, 14), [3, 5]),
    ((5, 1), [6, 4]),
    ((5, 4), [1, 6]),
    ((5, 6), [4, 1]),
    ((6, 5), [7, 15]),
    ((6, 7), [15, 5]),
    ((6, 15), [5, 7]),
    ((7, 6), [8, 17]),
    ((7, 8), [17, 6]),
    ((7, 17), [6, 8]),
    ((8, 1), [9, 7]),
    ((8, 7), [1, 9]),
    ((8, 9), [7, 1]),
    ((9, 8), [10, 18]),
    ((9, 10), [18, 8]),
    ((9, 18), [8, 10]),
    ((10, 2), [11, 9]),
    ((10, 9), [2, 11]),
    ((10, 11), [9, 2]),
    ((11, 10), [12, 19]),
    ((11, 12), [19, 10]),
    ((11, 19), [10, 12]),
    ((12, 3), [13, 11]),
    ((12, 11), [3, 13]),
    ((12, 13), [11, 3]),
    ((13, 12), [14, 20]),
    ((13, 14), [20, 12]),
    ((13, 20), [12, 14]),
    ((14, 4), [15, 13]),
    ((14, 13), [4, 15]),
    ((14, 15), [13, 4]),
    ((15, 6), [16, 14]),
    ((15, 14), [6, 16]),
    ((15, 16), [14, 6]),
    ((16, 15), [17, 20]),
    ((16, 17), [20, 15]),
    ((16, 20), [15, 17]),
    ((17, 7), [18, 16]),
    ((17, 16), [7, 18]),
    ((17, 18), [16, 7]),
    ((18, 9), [19, 17]),
    ((18, 17), [9, 19]),
    ((18, 19), [17, 9]),
    ((19, 11), [20, 18]),
    ((19, 18), [11, 20]),
    ((19, 20), [18, 11]),
    ((20, 13), [16, 19]),
    ((20, 16), [19, 13]),
    ((20, 19), [13, 16])
  ]
