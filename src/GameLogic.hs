import Types
import Data.List(elemIndex, find)
import Data.Maybe (fromMaybe)
import System.Random.Shuffle
import System.Random
-- CaveLayout -> Current Position -> Last Position -> Move -> Position
move :: CaveLayout -> Position -> Position -> Move -> Position
-- Example of how last postion is helpful:
-- let current position be 1
-- let last position be 2
-- let 1's mapLayout entry be (1, [2, 5, 8]),
-- Move Back -> obivously return 2
-- Move Left -> move left of 2 (cyclically if out of bounds) return 8
-- Move Right -> move right of 2 return 5

-- this assumes that the map is bidirectional else the transformation will start
move layout startPos prevPos moveTo = neighbors !! ( (moveTransform + matchingIndex) `mod` length neighbors )
    where 
        neighbors = layout !! startPos
        matchingIndex = fromMaybe 0 (elemIndex prevPos neighbors)
        moveTransform = case moveTo of
            Types.Left -> -1
            Types.Back -> 0
            Types.Right -> 1

type MoveInMap = Position -> Position -> Move -> Position
-- Map Layout:
decahedron :: CaveLayout
decahedron = [[1, 4, 7],   -- 0
             [0, 2, 9],    -- 1
             [1, 3, 11],   -- 2
             [2, 4, 13],   -- 3
             [0, 3, 5],    -- 4
             [4, 6, 14],   -- 5
             [5, 7, 16],   -- 6
             [0, 6, 8],    -- 7
             [7, 9, 17],   -- 8
             [1, 8, 10],   -- 9
             [9, 11, 19],  -- 10
             [2, 10, 12],  -- 11
             [11, 13, 19], -- 12
             [3, 12, 14],  -- 13
             [5, 13, 15],  -- 14
             [14, 16, 19], -- 15
             [6, 15, 17],  -- 16
             [8, 16, 18],  -- 17
             [10, 17, 19], -- 18
             [12, 15, 18]] -- 19

moveInDecahedron :: MoveInMap
moveInDecahedron = move decahedron

data StartGameState = StartGameState
  { playerCurrentPosition :: Position,
    -- will have to be set to the correct position on game start
    --    to orientate player
    playerLastPostion :: Position,
    playerArrowCount :: Int,
    caveLayout :: CaveLayout,
    numberOfBats :: Int,
    numberOfPits :: Int,
    startRandomGen :: StdGen
  }

createStartState :: StartGameState -> GameState
createStartState startGameState = GameState
    { 
        playerState = PlayerState { 
            currentPosition = playerCurrentPosition startGameState,
            lastPosition = playerLastPostion startGameState,
            arrowCount = playerLastPostion startGameState
        },
      wumpusState = WumpusState { wumpusPosition = wumpusPos },
      environmentState = EnvironmentState { hazards = envHazards },
      randomGen = nextGen
    }
    where 
        indices = [0..(length (caveLayout startGameState))]
        (num, onceGen) = uniform (startRandomGen startGameState)
        (stateGen, nextGen) = split onceGen
        shuffledIndices = shuffle' indices num stateGen
        -- make it so that the wumpus/ hazards cannot be at the start
        -- Maybe it makes since that they also cannot be neighbors but that is not implemented
        (wumpusPos:restIndices) = filter (/= 0) shuffledIndices
        hazardsList = replicate (numberOfBats startGameState) Bats 
            ++ replicate (numberOfBats startGameState) Pit
        envHazards = zip restIndices hazardsList

-- main goal is to check for hazard / wumpus
onEnterNewRoom :: GameState -> GameState
onEnterNewRoom = undefined
