module Functions where
import Types

import Data.Maybe (mapMaybe)
import System.Random (randomRIO)
import Control.Monad.State



smell :: GameState -> String
smell game = 
  let playerPos = playerPosition $ playerState game
      env       = environment game
      wumpusNearby = wumpusLocation env `elem` getNeighbors playerPos (layout game)
  in if wumpusNearby
    then "You smell something foul nearby..."
    else "You smell nothing unusual."


listen :: GameState -> String
listen game = 
  let playerPos = playerPosition $ playerState game
      env   = environment game
      neighbors = getNeighbors playerPos (layout game)
      batSounds = any (`elem` batsLocations env) neighbors
      pitSounds = any (`elem` pitsLocations env) neighbors
      sounds =
        ["fluttering of wings" | batSounds ] ++ 
        ["whirring wind" | pitSounds]
  in if null sounds
      then "You hear nothing unusual."
      else "You hear the " ++ unwords sounds



shoot :: String -> StateT GameState IO String
shoot direction = do 
  game <- get -- access current game state
  let playerPos = playerPosition $ playerState game
      env       = environment game
      gameLayout    = layout game -- needed to call it gameLayout to get beyond naming conflict
      previous  = lastPostion $ playerState game
      neighbors = getOrientationAdjustedNeighbors playerPos previous gameLayout
      targetCave = case direction of
        "Back"  -> neighbors !! 0     
        "Right" -> neighbors !! 1
        "Left"  -> neighbors !! 2
        _       -> error "Invalid direction"
      arrowsLeft = playerArrowCount $ playerState game  -- get arrrows from playerState
      wumpusNearby = wumpusLocation env `elem` neighbors -- check if wumpus is in neighboring cave, otherwise why would it run
  if arrowsLeft <= 0
    then return "You're out of arrows! Better find your way back out!" -- Would be cool to let player win by going back to cave 1
    else do
      -- updatedPlayer with new arrow amount
      let updatedPlayer = (playerState game) {playerArrowCount = arrowsLeft - 1} 
          updatedGame = game {playerState = updatedPlayer}
      put updatedGame
    
      if targetCave == wumpusLocation env
        then return "You killed the Wumpus! You win!"
        else if wumpusNearby
          then do
            newWumpusPos <- liftIO $ randomRIO (1, 20) -- need to lift into StateT monad bc doesn't support IO inherently
            let updatedEnv = env {wumpusLocation = newWumpusPos}
            put $ game { playerState = updatedPlayer, environment = updatedEnv }
            return "You missed! The Wumpus has scurried away in fear!"
          else do
            put $ game {playerState = updatedPlayer}
            return ("You missed! With only " ++ show arrowsLeft ++ " arrows left, I'd recommend you be extra careful... You never what lies waiting for you around the next corne--")
  


-- Rewrote randomizer/random so that it more easily integrates with the data and type definitions we already have
randomizeEnvironment :: [Position] -> IO EnvironmentState
randomizeEnvironment roomIds = do
  wumpusRoom  <- randomPick roomIds
  batRooms    <- randomPicks roomIds [wumpusRoom] 2
  pitRooms    <- randomPicks roomIds (wumpusRoom : batRooms) 2
  return EnvironmentState
    {
      wumpusLocation = wumpusRoom,
      batsLocations = batRooms,
      pitsLocations = pitRooms
    }


randomPick :: [Position] -> IO Position
randomPick roomIds = do
  index <- randomRIO (1, length roomIds - 1)
  return (roomIds !! index)


randomPicks :: [Position] -> [Position] -> Int -> IO [Position]
randomPicks roomIds exclude n = do
  let available = filter (`notElem` exclude) roomIds
  if n > length available
    then error "Somehow not enough rooms"
    else mapM (const $ randomPick available) [1..n] -- recall what mapM and const are used for when 




{-

assignRandomFeatures :: [RoomId] -> IO [(RoomId, RoomFeature)]
assignRandomFeatures roomIds = do
      wumpusRoom <- randomPick roomIds
      batRooms <- randomPicks roomIds [wumpusRoom] 2
      pitRooms <- randomPicks roomIds (wumpusRoom : batRooms) 2
      let hazards = [(wumpusRoom, Wumpus)]
                    ++ [(r, Bat) | r <- batRooms]
                    ++ [(r, Breeze) | r <- pitRooms]
      return hazards

randomPick :: [RoomId] -> IO RoomId
randomPick roomIds = do
  index <- randomRIO (0, length roomIds - 1)
  return (roomIds !! index)

randomPicks :: [RoomId] -> [RoomId] -> Int -> IO [RoomId]
randomPicks roomIds exclude n = do
  let available = filter (`notElem` exclude) roomIds
  if n > length available
    then error "Not enough available rooms for selection!"
    else do
      picked <- mapM (const $ randomPick available) [1 .. n]
      return picked

-}



{-

senseRoom :: Room -> String
senseRoom room =
  case roomFeature room of
    Wumpus -> "WUMPUS NEAR BY!"
    Bat    -> "Flutters"
    Breeze -> "Breeze!"
    Empty  -> "The room is empty."


  
-}



