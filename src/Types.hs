{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
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

data Action = MoveAction Move | ShootAction [Position] deriving (Show, Eq)

data GameState = GameState
  { playerState      :: PlayerState,
    wumpusState      :: WumpusState,
    environmentState :: EnvironmentState,
    gen              :: StdGen,
    gameStatus       :: GameStatus
  } deriving (Show, Eq)

-- will have to be set to the correct position on game start to orientate player
data PlayerState = Player
  { playerPosition :: Position,
    lastPosition :: Position,
    playerArrowCount :: Int,
    playerHasShot :: Bool
  } deriving (Show, Eq)

data WumpusState = WumpusState
  { wumpusPosition :: Position
  } deriving (Show, Eq)

data EnvironmentState = EnvironmentState
  { hazards :: [(Position, Hazard)]
  } deriving (Show, Eq)

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
      let
        wumpusPos = wumpusPosition $ wumpusState gameState
        hazardMap = hazards $ environmentState gameState
        hazardAt n = lookup n hazardMap
      in
        concatMap (senseForNeighbor pos) neighbors ++ wumpusSense pos neighbors wumpusPos

    senseForNeighbor :: Position -> Position -> [Sense]
    senseForNeighbor current neighbor =
      case lookup neighbor (hazards $ environmentState gameState) of
        Just Bats -> [HearBats]
        Just Pit  -> [FeelDraft]
        Nothing   -> []

    wumpusSense :: Position -> [Position] -> Position -> [Sense]
    wumpusSense current neighbors wumpusPos
      | wumpusPos == current = []  -- Wumpus is in the same cave; player will see it
      | wumpusPos `elem` neighbors = [SmellWumpus]
      | otherwise = []

displaySenses :: Position -> [(Position, [Sense])] -> IO ()
displaySenses current senses =
  case lookup current senses of
    Just senseList -> mapM_ (putStrLn . describeSense) senseList
    Nothing -> return ()

describeSense :: Sense -> String
describeSense SmellWumpus = "You smell something foul nearby."
describeSense HearBats    = "You hear the flapping of wings."
describeSense FeelDraft   = "You feel a draft nearby."

--makeCircular :: (Ord a, Int a) => [a] -> [a]
--makeCircular = fix $ ++

-- Map Layout:
--decahedron :: CaveLayout
--decahedron = -- [Left, Right, Back]
{-  
  [ (1, makeCircular [2, 5, 8]),
    (2, makeCircular [3, 1, 10]),
    (3, makeCircular [4, 2, 12]),
    (4, makeCircular [5, 3, 14]),
    (5, makeCircular [1, 4, 6]),
    (6, makeCircular [7, 15, 5]),
    (7, makeCircular [8, 6, 17]),
    (8, makeCircular [9, 7, 1]),
    (9, makeCircular [10, 8, 18]),
    (10, makeCircular [11, 9, 2]),
    (11, makeCircular [12, 10, 19]),
    (12, makeCircular [13, 11, 3]),
    (13, makeCircular [14, 12, 20]),
    (14, makeCircular [15, 13, 4]),
    (15, makeCircular [6, 14, 10]),
    (16, makeCircular [17, 20, 15]),
    (17, makeCircular [18, 16, 7]),
    (18, makeCircular [19, 17, 9]),
    (19, makeCircular [20, 18, 11]),
    (20, makeCircular [16, 19, 13])
  ]
 -}
