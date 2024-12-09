module GameLogic where
import Types
import Data.List(elemIndex, find)
import Data.Maybe (fromMaybe)
import System.Random.Shuffle
import System.Random
import IO
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
          arrowCount = playerArrowCount startGameState
      },
      wumpusState = WumpusState {
          wumpusPosition = wumpusPos
          },
      environmentState = EnvironmentState { hazards = envHazards },
      randomGen = nextGen,
      mover = move (caveLayout startGameState),
      isAlive = True,
      lastPosFinder = (!!) (head (caveLayout startGameState))
    }
    where
        indices = [0..(length (caveLayout startGameState)-1)]
        (stateGen, nextGen) = split (startRandomGen startGameState)
        shuffledIndices = shuffle' indices (length (caveLayout startGameState)) stateGen
        -- make it so that the wumpus/ hazards cannot be at the start
        -- Maybe it makes since that they also cannot be neighbors but that is not implemented
        (wumpusPos:restIndices) = filter (/= 0) shuffledIndices
        lastWumpusPos = head (caveLayout startGameState !! wumpusPos)
        hazardsList = replicate (numberOfBats startGameState) Bats
            ++ replicate (numberOfBats startGameState) Pit
        envHazards = zip restIndices hazardsList

-- main goal is to check for hazard / wumpus
onEnterNewRoom :: GameState -> GameState
onEnterNewRoom = undefined


shootArrow :: IO [Move]
shootArrow = collectMoves 5 []  -- Start with an empty list and a max count of 5

-- Helper function to recursively collect moves
collectMoves :: Int -> [Move] -> IO [Move]
collectMoves 0 moves = return moves  -- Stop when the maximum number of moves is reached
collectMoves remaining moves = do
    putStrLn $ "Arrow moves collected so far: " ++ show moves
    putStrLn $ "The arrow can go " ++ show remaining ++ " more rooms."
    move <- getArrowMoveFromUser
    case move of
        Nothing -> return moves -- return the list smaller than 5
        Just validMove -> collectMoves (remaining - 1) (moves ++ [validMove])  -- Append the move and decrement the counter


updateArrowCount :: GameState -> GameState
updateArrowCount gameState =
    let
        currentArrows = arrowCount (playerState gameState)
        updatedArrows = max 0 (currentArrows - 1)  -- Ensure the arrow count does not go below 0
    in
        gameState {
            playerState = (playerState gameState) {
                arrowCount = updatedArrows
            }
        }
