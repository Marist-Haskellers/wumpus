module Lib
  (
    someFunc,
    selectRandomElement,
    getRandomPosition,
    move,
    setState,
    handleMovement,
    handleShooting,
    testCaveLayout,
    testCaveMap
  ) where

import qualified System.Random as Random
--import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Old Dodecahedron map to test with
testCaveLayout :: CaveLayout
testCaveLayout =
  [ (1,  [2, 5, 8]),
    (2,  [1, 3, 10]),
    (3,  [2, 4, 12]),
    (4,  [3, 5, 14]),
    (5,  [1, 4, 6]),
    (6,  [5, 7, 15]),
    (7,  [6, 8, 17]),
    (8,  [1, 7, 9]),
    (9,  [8, 10, 18]),
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

testCaveMap :: MoveLayout
testCaveMap =
  [ ((1,2), [8,5]),
    ((1,5), [2,8]),
    ((1,8), [5,2]),
    ((2,1), [3,10]),
    ((2,3), [10,1]),
    ((2,10), [1,3]),
    ((3,2), [4,12]),
    ((3,4), [12,2]),
    ((3,12), [2,4]),
    ((4,3), [5,14]),
    ((4,5), [14,3]),
    ((4,14), [3,5]),
    ((5,1), [6,4]),
    ((5,4), [1,6]),
    ((5,6), [4,1]),
    ((6,5), [7,15]),
    ((6,7), [15,5]),
    ((6,15), [5,7]),
    ((7,6), [8,17]),
    ((7,8), [17,6]),
    ((7,17), [6,8]),
    ((8,1), [9,7]),
    ((8,7), [1,9]),
    ((8,9), [7,1]),
    ((9,8), [10,18]),
    ((9,10), [18,8]),
    ((9,18), [8,10]),
    ((10,2), [11,9]),
    ((10,9), [2,11]),
    ((10,11), [9,2]),
    ((11,10), [12,19]),
    ((11,12), [19,10]),
    ((11,19), [10,12]),
    ((12,3), [13,11]),
    ((12,11), [3,13]),
    ((12,13), [11,3]),
    ((13,12), [14,20]),
    ((13,14), [20,12]),
    ((13,20), [12,14]),
    ((14,4), [15,13]),
    ((14,13), [4,15]),
    ((14,15), [13,4]),
    ((15,6), [16,14]),
    ((15,14), [6,16]),
    ((15,16), [14,6]),
    ((16,15), [17,20]),
    ((16,17), [20,15]),
    ((16,20), [15,17]),
    ((17,7), [18,16]),
    ((17,16), [7,18]),
    ((17,18), [16,7]),
    ((18,9), [19,17]),
    ((18,17), [9,19]),
    ((18,19), [17,9]),
    ((19,11), [20,18]),
    ((19,18), [11,20]),
    ((19,20), [18,11]),
    ((20,13), [16,19]),
    ((20,16), [19,13]),
    ((20,19), [13,16])
  ]

-- Moves the player, in main this would be called before setState
-- CaveLayout, currentPosition, previousPosition, noveDirection, new position
{-move :: MoveLayout -> Position -> Position -> Move -> Position
move layout currentPosition previousPosition moveType =
  case lookup [currentPosition, previousPosition] layout of
    Just [left, right] ->
      case moveType of
        MoveBack  -> previousPosition -- Moving back to the previous position
        MoveLeft  -> left             -- Moving to the left cave
        MoveRight -> right            -- Moving to the right cave
    Nothing -> error $ "No valid move found for currentPosition: " ++ show currentPosition
                    ++ ", previousPosition: " ++ show previousPosition -}

move :: MoveLayout -> Position -> Position -> Move -> Position
move layout currentPosition previousPosition moveType =
  case lookup (currentPosition, previousPosition) layout of
    Just [left, right] ->
      case moveType of
        MoveBack  -> previousPosition -- Move back to the previous position
        MoveLeft  -> left             -- Move to the left cave
        MoveRight -> right            -- Move to the right cave
    Nothing -> error $ "No valid move found for currentPosition: " ++ show currentPosition
                    ++ ", previousPosition: " ++ show previousPosition


-- Function that modifies the state of the game depending on the players actions
setState :: PlayerState -> Action -> GameState -> GameState
setState player action gameState =
  case action of
    MoveAction moveDir ->
      -- Existing movement logic
      handleMovement player moveDir gameState
    ShootAction path ->
      -- Shooting logic
      handleShooting player path gameState

-- Function to handle movement of player
-- Take in current player state, action they performed, curent game state, and gives new game state
handleMovement :: PlayerState -> Move -> GameState -> GameState
handleMovement player moveDir gameState =
  let
    -- Extract fields from GameState
    wumpus = wumpusState gameState
    env = environmentState gameState
    genVal = gen gameState
    --status = gameStatus gameState     This will be used once main and game is working

    currentPos = playerPosition player
    lastPos = lastPosition player

    hazardsList = hazards env
    wumpusPos = wumpusPosition wumpus

    -- Determine new position based on moveDir
    newPos = move testCaveMap currentPos lastPos moveDir

    -- Update PlayerState
    updatedPlayer = player {
      lastPosition = currentPos,
      playerPosition = newPos,
      playerHasShot = False     -- Reset shot status everytime the player moves
    }

    -- Check for hazards at the new position
    hazard = lookup newPos hazardsList

    -- Check if player has encountered the Wumpus
    wumpusEncounter = newPos == wumpusPos

    -- Handle hazard effects
    (interimPlayer, interimGen, interimStatus) = case hazard of
      Just Bats ->
        -- Transport player to a random position
        let (randPos, newStdGen) = getRandomPosition genVal testCaveLayout
            transportedPlayer = updatedPlayer {
              playerPosition = randPos,
              lastPosition = newPos
            }
        in (transportedPlayer, newStdGen, Ongoing)
      Just Pit ->
        -- Player falls into a pit, game over
        (updatedPlayer, genVal, GameOver "You fell into a pit!")
      Nothing ->
        -- No hazard encountered
        (updatedPlayer, genVal, Ongoing)

    -- Handle Wumpus encounter with random chance
    (finalPlayer, finalWumpus, finalGen, finalStatus) = if wumpusEncounter
      then
        let (chance, newStdGen) = Random.randomR (1, 2) interimGen :: (Int, Random.StdGen) -- 1: Player dies, 2: Wumpus flees
        in if chance == 1
           then
             -- Player dies
             (interimPlayer, wumpus, newStdGen, GameOver "You were eaten by the Wumpus!")
           else
             -- Wumpus flees to a random adjacent cave
             let connections = fromMaybe [] (lookup wumpusPos testCaveLayout)
                 (idx, updatedGen') = Random.randomR (0, length connections - 1) newStdGen :: (Int, Random.StdGen)
                 newWumpusPos = connections !! idx
                 newWumpus = wumpus { wumpusPosition = newWumpusPos }
             in (interimPlayer, newWumpus, updatedGen', Ongoing)
      else
        -- No Wumpus encounter
        (interimPlayer, wumpus, interimGen, interimStatus)

    -- Determine the final game status
    finalGameStatus = finalStatus
  in
    -- Return the updated GameState
    gameState {
      playerState = finalPlayer,
      wumpusState = finalWumpus,
      gen = finalGen,
      gameStatus = finalGameStatus
    }

handleShooting :: PlayerState -> [Position] -> GameState -> GameState
handleShooting player path gameState =
  let
    genVal = gen gameState
    wumpus = wumpusState gameState
    wumpusPos = wumpusPosition wumpus
    remainingArrows = playerArrowCount player - 1

    -- Check if the player has arrows left
    gameStatus' = if remainingArrows < 0
                  then GameOver "You have no arrows left!"
                  else gameStatus gameState

    -- Check if the arrow path exceeds the maximum length
    gameStatus'' = if length path > 5
                   then GameOver "Your arrow cannot travel more than 5 caves!"
                   else gameStatus'

    -- Validate the arrow path
    isValidPath = validateArrowPath (playerPosition player) path

    -- Update game status if path is invalid
    gameStatus''' = if not isValidPath
                    then GameOver "Invalid arrow path!"
                    else gameStatus''

    -- Determine if the arrow hits the Wumpus
    arrowHitsWumpus = wumpusPos `elem` path

    -- Update game status if the Wumpus is hit
    finalGameStatus = if arrowHitsWumpus
                      then GameOver "You have killed the Wumpus! You win!"
                      else gameStatus'''

    -- If the arrow misses and the game is ongoing, the Wumpus may move
    (updatedWumpusState, finalGen) = if not arrowHitsWumpus && finalGameStatus == Ongoing
                                     then moveWumpusRandomly wumpus genVal
                                     else (wumpus, genVal)

    -- Update the player's arrow count
    updatedPlayer = player { playerArrowCount = remainingArrows, playerHasShot = True }

  in
    gameState {
      playerState = updatedPlayer,
      wumpusState = updatedWumpusState,
      gen = finalGen,
      gameStatus = finalGameStatus
    }

validateArrowPath :: Position -> [Position] -> Bool
validateArrowPath _ [] = True  -- Empty path is valid
validateArrowPath currentPos (nextPos:rest) =
  case lookup currentPos testCaveLayout of
    Just connections ->
      (nextPos `elem` connections) && validateArrowPath nextPos rest
    Nothing -> False  -- Current position not found in the cave layout


-- Helper function that generates a random position, used for Bats and Wumpus moving
selectRandomElement :: Random.StdGen -> [a] -> (a, Random.StdGen)
selectRandomElement genValue list =
  let
    (idx, newStdGen) = Random.randomR (0, length list - 1) genValue
    element = list !! idx
  in
    (element, newStdGen)

-- Used for bats transporting player to a random cave
getRandomPosition :: Random.StdGen -> CaveLayout -> (Position, Random.StdGen)
getRandomPosition genValue cave =
  let
    allPositions = map fst cave
    (newPos, newStdGen) = selectRandomElement genValue allPositions
  in
    (newPos, newStdGen)

moveWumpusRandomly :: WumpusState -> Random.StdGen -> (WumpusState, Random.StdGen)
moveWumpusRandomly currentWumpusState genValue =
  let
    wumpusPos = wumpusPosition currentWumpusState
    connections = fromMaybe [] (lookup wumpusPos testCaveLayout)
    (newWumpusPos, newGen) = selectRandomElement genValue connections
    updatedWumpus = currentWumpusState { wumpusPosition = newWumpusPos }
  in
    (updatedWumpus, newGen)
