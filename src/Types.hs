module Types (module Types) where

import Data.Foldable (forM_)
import System.Random (StdGen)

type Position = Int

-- Game status indicating whether the game is ongoing or has ended
data GameStatus
  = Ongoing String -- Includes a status message
  | GameOver String -- Includes the reason for the game's end
  deriving (Show, Eq)

-- Player movements relative to their current cave
data Move
  = MoveLeft
  | MoveRight
  | MoveBack
  deriving (Show, Eq)

-- if move is an enum then it forces the game to have
--    # of enums amount of connections per cave
--    a more generic way of representing moves could be
--    used but it may result it move names which are bland (e.i. to room 3)
--    or move names which are incorrect (e.i. moving left when in that position you can only move right)
-- For a decahedron it Left Right Back make sense for every move as you will always have those options
--    if orientated correctly

-- Player actions within the game
data Action
  = MoveAction Move
  | ShootAction [Position] -- Sequence of positions to shoot the arrow through
  | SenseAction Sense -- Action to sense hazards in the current cave
  deriving (Show, Eq)

-- GameState holds all the information about the current state of the game
data GameState = GameState
  { playerState :: PlayerState,
    wumpusState :: WumpusState,
    environmentState :: EnvironmentState,
    gen :: StdGen,
    gameStatus :: GameStatus,
    caveLayout :: CaveLayout, -- Layout of the cave system
    moveLayout :: MoveLayout -- Layout for move transitions
  }
  deriving (Show, Eq)

-- State of the player within the game
data PlayerState = Player
  { playerPosition :: Position, -- Current position of the player
    lastPosition :: Position, -- Previous position of the player
    playerArrowCount :: Int, -- Number of arrows remaining
    playerHasShot :: Bool -- Whether the player has shot an arrow
  }
  deriving (Show, Eq)

-- State of the Wumpus in the game
newtype WumpusState = WumpusState
  { wumpusPosition :: Position -- Current position of the Wumpus
  }
  deriving (Show, Eq)

-- State of the environment, including hazards
newtype EnvironmentState = EnvironmentState
  { hazards :: [(Position, Hazard)] -- List of hazards and their positions
  }
  deriving (Show, Eq)

-- Types of hazards in the cave system
data Hazard
  = Bats -- Bats transport the player to a random cave
  | Pit -- Falling into a pit ends the game
  | Wumpus -- Getting eaten by the Wumpus ends the game
  deriving (Show, Eq)

-- Layout of caves, mapping each position to its connections
type CaveLayout = [(Position, [Position])]

-- Layout for move transitions, specifying allowable moves between caves
type MoveLayout = [((Position, Position), [Position])]

-- Types of senses the player can use to detect hazards
data Sense
  = SmellWumpus -- Detect the Wumpus's smell
  | HearBats -- Detect the flapping of bats
  | FeelDraft -- Detect drafts near pits
  deriving (Show, Eq)

-- Generate sense data for the entire cave layout, filtering by the specific sense
generateSenseData :: CaveLayout -> GameState -> [(Position, [Sense])]
generateSenseData layout gameState =
  map (\(pos, neighbors) -> (pos, sensesForCave pos neighbors)) layout
  where
    sensesForCave :: Position -> [Position] -> [Sense]
    sensesForCave pos neighbors =
      let wumpusPos = wumpusPosition $ wumpusState gameState
       in concatMap senseForNeighbor neighbors ++ wumpusSense pos neighbors wumpusPos

    senseForNeighbor :: Position -> [Sense]
    senseForNeighbor neighbor =
      case lookup neighbor (hazards $ environmentState gameState) of
        Just Bats -> [HearBats]
        Just Pit -> [FeelDraft]
        Just Wumpus -> [SmellWumpus]
        Nothing -> []

    wumpusSense :: Position -> [Position] -> Position -> [Sense]
    wumpusSense current neighbors wumpusPos
      | wumpusPos == current = [] -- Wumpus is in the same cave; player will see it
      | wumpusPos `elem` neighbors = [SmellWumpus]
      | otherwise = []

-- Display sensed hazards in the current cave
displaySenses :: Position -> [(Position, [Sense])] -> IO ()
displaySenses current senses =
  Data.Foldable.forM_
    (lookup current senses)
    (mapM_ (putStrLn . describeSense))

-- Describe individual senses
describeSense :: Sense -> String
describeSense SmellWumpus = "You smell something foul nearby."
describeSense HearBats = "You hear the flapping of wings."
describeSense FeelDraft = "You feel a draft nearby."

-- | Decahedron layout: The default cave system with 20 interconnected caves.
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

-- | Movement map for the decahedron layout.
decahedronMap :: MoveLayout
decahedronMap =
  [ ((1, 2), [5, 8]),
    ((1, 5), [2, 8]),
    ((1, 8), [2, 5]),
    ((2, 1), [3, 10]),
    ((2, 3), [1, 10]),
    ((2, 10), [1, 3]),
    ((3, 2), [4, 12]),
    ((3, 4), [2, 12]),
    ((3, 12), [2, 4]),
    ((4, 3), [5, 14]),
    ((4, 5), [3, 14]),
    ((4, 14), [3, 5]),
    ((5, 1), [4, 6]),
    ((5, 4), [1, 6]),
    ((5, 6), [1, 4]),
    ((6, 5), [7, 15]),
    ((6, 7), [5, 15]),
    ((6, 15), [5, 7]),
    ((7, 6), [8, 17]),
    ((7, 8), [6, 17]),
    ((7, 17), [6, 8]),
    ((8, 1), [7, 9]),
    ((8, 7), [1, 9]),
    ((8, 9), [1, 7]),
    ((9, 8), [10, 18]),
    ((9, 10), [8, 18]),
    ((9, 18), [8, 10]),
    ((10, 2), [9, 11]),
    ((10, 9), [2, 11]),
    ((10, 11), [2, 9]),
    ((11, 10), [12, 19]),
    ((11, 12), [10, 19]),
    ((11, 19), [10, 12]),
    ((12, 3), [11, 13]),
    ((12, 11), [3, 13]),
    ((12, 13), [3, 11]),
    ((13, 12), [14, 20]),
    ((13, 14), [12, 20]),
    ((13, 20), [12, 14]),
    ((14, 4), [13, 15]),
    ((14, 13), [4, 15]),
    ((14, 15), [4, 13]),
    ((15, 6), [14, 16]),
    ((15, 14), [6, 16]),
    ((15, 16), [6, 14]),
    ((16, 15), [17, 20]),
    ((16, 17), [15, 20]),
    ((16, 20), [15, 17]),
    ((17, 7), [16, 18]),
    ((17, 16), [7, 18]),
    ((17, 18), [7, 16]),
    ((18, 9), [17, 19]),
    ((18, 17), [9, 19]),
    ((18, 19), [9, 17]),
    ((19, 11), [18, 20]),
    ((19, 18), [11, 20]),
    ((19, 20), [11, 18]),
    ((20, 13), [16, 19]),
    ((20, 16), [13, 19]),
    ((20, 19), [13, 16])
  ]

-- | Cool Hexagon layout: A visually interesting larger map with 12 caves.
hexagon :: CaveLayout
hexagon =
  [ (1, [2, 6, 12]),
    (2, [1, 3, 7]),
    (3, [2, 4, 8]),
    (4, [3, 5, 9]),
    (5, [4, 6, 10]),
    (6, [1, 5, 11]),
    (7, [2, 8, 12]),
    (8, [3, 7, 9]),
    (9, [4, 8, 10]),
    (10, [5, 9, 11]),
    (11, [6, 10, 12]),
    (12, [1, 7, 11])
  ]

-- | Movement map for the hexagon layout.
hexagonMap :: MoveLayout
hexagonMap =
  [ ((1, 2), [6, 12]),
    ((1, 6), [2, 12]),
    ((1, 12), [2, 6]),
    ((2, 1), [3, 7]),
    ((2, 3), [1, 7]),
    ((2, 7), [1, 3]),
    ((3, 2), [4, 8]),
    ((3, 4), [2, 8]),
    ((3, 8), [2, 4]),
    ((4, 3), [5, 9]),
    ((4, 5), [3, 9]),
    ((4, 9), [3, 5]),
    ((5, 4), [6, 10]),
    ((5, 6), [4, 10]),
    ((5, 10), [4, 6]),
    ((6, 1), [5, 11]),
    ((6, 5), [1, 11]),
    ((6, 11), [1, 5]),
    ((12, 1), [7, 11]),
    ((12, 7), [1, 11]),
    ((12, 11), [1, 7])
  ]

-- | Triangle layout: A more complex triangular system with 9 caves.
triangle :: CaveLayout
triangle =
  [ (1, [2, 3, 4]),
    (2, [1, 5, 6]),
    (3, [1, 6, 7]),
    (4, [1, 7, 8]),
    (5, [2, 8, 9]),
    (6, [2, 3, 9]),
    (7, [3, 4, 9]),
    (8, [4, 5, 9]),
    (9, [5, 6, 7, 8])
  ]

-- | Movement map for the triangle layout.
triangleMap :: MoveLayout
triangleMap =
  [ ((1, 2), [3, 4]),
    ((1, 3), [2, 4]),
    ((1, 4), [2, 3]),
    ((2, 1), [5, 6]),
    ((2, 5), [1, 6]),
    ((2, 6), [1, 5]),
    ((9, 8), [5, 6, 7]),
    ((9, 7), [6, 8]),
    ((9, 6), [7, 8])
  ]
