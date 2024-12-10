module Types where
import System.Random

type Position = Int

-- if move is an enum then it forces the game to have
--    # of enums amount of connections per cave
--    a more generic way of representing moves could be
--    used but it may result it move names which are bland (e.i. to room 3)
--    or move names which are incorrect (e.i. moving left when in that postion you can only move right)
-- For a decahedron it Left Right Back make sense for every move as you will always have those options
--    if orientated correctly
data Move = Left | Right | Back | Stop deriving(Show, Eq, Enum, Bounded)
-- implementation of random for Move (I assume it would work)
instance Random Move where
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
    (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

type MoveInMap = Position -> Position -> Move -> Position

data Sense = Hear | Feel | Smell deriving(Show, Eq)

data Choice = ChoiceMove | ChoiceSense | ChoiceShoot deriving(Show)

data PlayerState = PlayerState
  { currentPosition :: Position,
    -- will have to be set to the correct position on game start
    --    to orientate player
    lastPosition :: Position,
    arrowCount :: Int
  }

data WumpusState = WumpusState
  { wumpusPosition :: Position
  }

data EnvironmentState = EnvironmentState
  { hazards :: [(Position, Hazard)]
  }

data GameState = GameState
    { playerState :: PlayerState,
      wumpusState :: WumpusState,
      environmentState :: EnvironmentState,
      mover :: MoveInMap,
      randomGen :: StdGen,
      isAlive :: Bool,
      -- to find some last postion for wumpus and
      -- when player gets moved
      lastPosFinder :: Position -> Position,
      amountOfRooms :: Int
    }



data Hazard = Bats | Pit

type CaveLayout = [[Position]]

