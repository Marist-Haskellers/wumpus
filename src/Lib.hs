module Lib (module Lib) where

import Data.Maybe (fromMaybe)
import System.Random as Random (Random (randomR), StdGen)
import Types
  ( Action (..),
    CaveLayout,
    EnvironmentState (hazards),
    GameState
      ( environmentState,
        gameStatus,
        gen,
        playerState,
        wumpusState
      ),
    GameStatus (GameOver, Ongoing),
    Hazard (Bats, Pit),
    Move (..),
    MoveLayout,
    PlayerState
      ( lastPosition,
        playerArrowCount,
        playerHasShot,
        playerPosition
      ),
    Position,
    Sense,
    WumpusState (wumpusPosition),
    describeSense,
    generateSenseData,
  )

-- Move function that is called during setState and handleMovement
move :: MoveLayout -> Position -> Position -> Move -> Position
move layout currentPosition previousPosition moveType =
  case lookup (currentPosition, previousPosition) layout of
    Just [left, right] ->
      case moveType of
        MoveBack -> previousPosition -- Move back to the previous position
        MoveLeft -> left -- Move to the left cave
        MoveRight -> right -- Move to the right cave
    _ -> error $ "\nInvalid move for currentPosition: " ++ show currentPosition ++ ", previousPosition: " ++ show previousPosition

-- Function to clean the state in the case of an invalid input
cleanState :: GameState -> GameState
cleanState state =
  state
    { gameStatus = case gameStatus state of
        Ongoing _ -> Ongoing "" -- Reset only ongoing messages
        other -> other -- Preserve game-over states
    }

-- Function that modifies the state of the game depending on the player's actions
setState :: PlayerState -> Action -> GameState -> CaveLayout -> MoveLayout -> GameState
setState player action gameState caveLayout moveLayout =
  case action of
    MoveAction moveDir -> handleMovement player moveDir gameState caveLayout moveLayout
    ShootAction path -> handleShooting player path gameState caveLayout
    SenseAction sense -> handleSensing player sense gameState caveLayout

-- Function to handle movement of the player
handleMovement :: PlayerState -> Move -> GameState -> CaveLayout -> MoveLayout -> GameState
handleMovement player moveDir gameState caveLayout moveLayout =
  let -- Extract fields from GameState
      wumpus = wumpusState gameState
      env = environmentState gameState
      genVal = gen gameState

      currentPos = playerPosition player
      lastPos = lastPosition player
      hazardsList = hazards env
      wumpusPos = wumpusPosition wumpus

      -- Determine new position based on moveDir
      newPos = move moveLayout currentPos lastPos moveDir

      -- Update PlayerState
      updatedPlayer =
        player
          { lastPosition = currentPos,
            playerPosition = newPos,
            playerHasShot = False -- Reset shot status every time the player moves
          }

      -- Check for hazards at the new position
      hazard = lookup newPos hazardsList

      -- Check if player has encountered the Wumpus
      wumpusEncounter = newPos == wumpusPos

      -- Handle hazard effects
      (interimPlayer, interimGen, interimStatus) = case hazard of
        Just Bats ->
          -- Transport player to a random position
          let (randPos, randLastPos, newStdGen) = getRandomPosition genVal caveLayout
              transportedPlayer =
                updatedPlayer
                  { playerPosition = randPos,
                    lastPosition = randLastPos
                  }
           in (transportedPlayer, newStdGen, Ongoing ("\nYou're in cave #" ++ show newPos ++ ". \nThe bats carried you to a random cave!"))
        Just Pit ->
          -- Player falls into a pit, game over
          (updatedPlayer, genVal, GameOver "\nYou fell into a pit!")
        Just _ -> (updatedPlayer, genVal, Ongoing "")
        Nothing ->
          -- No hazard encountered
          (updatedPlayer, genVal, Ongoing "")

      -- Handle Wumpus encounter
      (finalPlayer, finalWumpus, finalGen, finalStatus) =
        if wumpusEncounter
          then
            let (chance, newStdGen) = Random.randomR (1, 2) interimGen :: (Int, StdGen) -- 1: Player dies, 2: Wumpus flees
             in if chance == 1
                  then
                    -- Player dies
                    (interimPlayer, wumpus, newStdGen, GameOver "\nYou were eaten by the Wumpus!")
                  else
                    -- Wumpus flees to a random adjacent cave
                    let connections = fromMaybe [] (lookup wumpusPos caveLayout)
                        (idx, updatedGen') = Random.randomR (0, length connections - 1) newStdGen :: (Int, StdGen)
                        newWumpusPos = connections !! idx
                        newWumpus = wumpus {wumpusPosition = newWumpusPos}
                     in (interimPlayer, newWumpus, updatedGen', Ongoing "\nThe Wumpus fled to another cave!")
          else
            -- No Wumpus encounter
            (interimPlayer, wumpus, interimGen, interimStatus)
   in -- Update the game state
      gameState
        { playerState = finalPlayer,
          wumpusState = finalWumpus,
          gen = finalGen,
          gameStatus = finalStatus
        }

-- Handle shooting logic
handleShooting :: PlayerState -> [Position] -> GameState -> CaveLayout -> GameState
handleShooting player path gameState caveLayout =
  let genVal = gen gameState
      wumpus = wumpusState gameState
      wumpusPos = wumpusPosition wumpus
      remainingArrows = playerArrowCount player - 1
      -- Determine the final game status based on various conditions
      finalGameStatus
        | remainingArrows < 0 = GameOver "\nYou have no arrows left!"
        | length path > 5 = GameOver "\nYour arrow cannot travel more than 5 caves!"
        | not (validateArrowPath caveLayout (playerPosition player) path) = GameOver "\nInvalid arrow path!"
        | wumpusPos `elem` path = GameOver "\nYou killed the Wumpus! You win!"
        | otherwise = Ongoing "Your arrow missed the Wumpus. It may have moved."

      -- Update Wumpus state if the arrow misses
      (updatedWumpusState, finalGen) =
        if finalGameStatus == Ongoing "Your arrow missed the Wumpus. It may have moved."
          then moveWumpusRandomly caveLayout wumpus genVal
          else (wumpus, genVal)

      updatedPlayer = player {playerArrowCount = remainingArrows, playerHasShot = True}
   in -- Update the player's arrow count
      gameState
        { playerState = updatedPlayer,
          wumpusState = updatedWumpusState,
          gen = finalGen,
          gameStatus = finalGameStatus
        }

-- Handle sensing logic
handleSensing :: PlayerState -> Sense -> GameState -> CaveLayout -> GameState
handleSensing player sense gameState caveLayout =
  let currentPos = playerPosition player
      senseData = generateSenseData caveLayout gameState
      message = case lookup currentPos senseData of
        Just senseList ->
          let filteredList = filter (== sense) senseList
           in if null filteredList
                then "There are no hazards nearby able to be sensed with this sense.\n"
                else unlines $ map describeSense filteredList
        Nothing -> "You sense nothing unusual.\n"
   in gameState {gameStatus = Ongoing message}

-- Validate if the arrow path is valid
validateArrowPath :: CaveLayout -> Position -> [Position] -> Bool
validateArrowPath _ _ [] = True
validateArrowPath caveLayout currentPos (nextPos : rest) =
  case lookup currentPos caveLayout of
    Just connections -> nextPos `elem` connections && validateArrowPath caveLayout nextPos rest
    Nothing -> False -- Current position not found in the cave layout

-- Random selection of an element from a list
selectRandomElement :: StdGen -> [a] -> (a, StdGen)
selectRandomElement genValue list =
  let (idx, newStdGen) = Random.randomR (0, length list - 1) genValue
   in (list !! idx, newStdGen)

-- Get a random position for the player or Wumpus
getRandomPosition :: StdGen -> CaveLayout -> (Position, Position, StdGen)
getRandomPosition genValue cave =
  let allPositions = map fst cave
      (newPos, gen1) = selectRandomElement genValue allPositions -- Random position
      connections = fromMaybe [] (lookup newPos cave)
      (lastPos, gen2) = selectRandomElement gen1 connections -- Random last position from connections
   in (newPos, lastPos, gen2)

moveWumpusRandomly :: CaveLayout -> WumpusState -> StdGen -> (WumpusState, StdGen)
moveWumpusRandomly caveLayout currentWumpusState genValue =
  let wumpusPos = wumpusPosition currentWumpusState
      connections = fromMaybe [] (lookup wumpusPos caveLayout)
      (newWumpusPos, newGen) = selectRandomElement genValue connections
      updatedWumpus = currentWumpusState {wumpusPosition = newWumpusPos}
   in (updatedWumpus, newGen)

-- I/O helper function to check if a cave has a hazard in it, for spawning the player with a random state
findSafePlayerPosition :: StdGen -> [(Position, Hazard)] -> CaveLayout -> (Position, Position, StdGen)
findSafePlayerPosition generator hazardsList cave =
  let unsafePositions = map fst hazardsList
      (playerPos, lastPos, newGen) = getRandomPosition generator cave
   in if playerPos `elem` unsafePositions
        then findSafePlayerPosition newGen hazardsList cave -- Retry until safe
        else (playerPos, lastPos, newGen)

-- Helper to find a safe position
getStartingPosition :: StdGen -> CaveLayout -> (Position, StdGen)
getStartingPosition generator caveLayout =
  let allPositions = map fst caveLayout
      (newPos, newGen) = selectRandomElement generator allPositions
   in (newPos, newGen)

-- I/O helper function to check if a cave has a hazard in it, for spawning the hazards in a ransom state
findSafePosition :: StdGen -> [(Position, Hazard)] -> CaveLayout -> (Position, StdGen)
findSafePosition generator hazardsList cave =
  let unsafePositions = map fst hazardsList
      (newPos, newGen) = getStartingPosition generator cave
   in if newPos `elem` unsafePositions
        then findSafePosition newGen hazardsList cave -- Retry until safe
        else (newPos, newGen)
