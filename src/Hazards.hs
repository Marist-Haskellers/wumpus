module Hazards where
import Types (Hazard(..), Sense(..), Position, CaveLayout, Move(..), GameState(..), PlayerState(..), WumpusState(..), EnvironmentState(..), Move)
import System.Random
import Data.List (find)

toSense :: Hazard -> Sense
toSense Bats = Hear
toSense Pit  = Feel


senseHazards :: GameState -> [Sense]
senseHazards gameState = do
    -- maybe there should be a better way to list movement options
    let movementOptions = [Types.Left, Types.Right, Types.Back];
    let currentPos = currentPosition (playerState gameState)
    let lastPos = lastPosition (playerState gameState)
    let moveInMap = mover gameState
    let neighbors = map (\move -> moveInMap currentPos lastPos move) movementOptions
    let senses = [Smell | wumpusPosition (wumpusState gameState) `elem` neighbors]
    let hzrds = hazards (environmentState gameState)
    senses ++ map (toSense . snd) (filter (\(pos, _) -> pos `elem` neighbors) hzrds) 



handleEnvironmentHazard :: GameState -> Maybe Hazard -> (Maybe String, GameState)
handleEnvironmentHazard gameState Nothing = (Nothing, gameState)
handleEnvironmentHazard gameState (Just Pit) = (
    Just "You fell into a pit! You die.", 
    gameState { isAlive=False }
    )
handleEnvironmentHazard gameState (Just Bats) = 
    let
        lastPlayerState = playerState gameState
        lastPos = lastPosition lastPlayerState
        -- move the player to a new room that is not the current room
        -- essentially mapping it to a move after it in index cycling back to 0 if going over
        -- the length
        oldGen = randomGen gameState
        (toAddIndices, newGen) = randomR (0, amountOfRooms gameState - 1) oldGen
        newPlayerPos = (toAddIndices + lastPos) `mod` amountOfRooms gameState
        finder = lastPosFinder gameState
        newLastPlayerPos = finder newPlayerPos
    in
        (
            Just "You are taken by bats to a new room!",
            gameState { 
                playerState=lastPlayerState {
                    currentPosition = newPlayerPos,
                    lastPosition = newLastPlayerPos
                },
                randomGen=newGen
            }
        )


handleEnvironmentHazards :: GameState -> (Maybe String, GameState)
handleEnvironmentHazards gameState =
    let
        hzrds = hazards (environmentState gameState)
        currentPos = currentPosition (playerState gameState)
    -- if you could one into multiple hazards at once this would need to be changed
        hzrd = fmap snd (find (\(pos, _) -> pos == currentPos) hzrds)
    in
        handleEnvironmentHazard gameState hzrd


handleWumpusInMyNoNoSquare :: GameState -> (Maybe String, GameState)
handleWumpusInMyNoNoSquare gameState =
    let 
        gen = randomGen gameState
        (doKill, newGen) = random gen :: (Bool, StdGen)
        (randomMove, newestGen) = random newGen :: (Move, StdGen)
        message = if doKill 
            then Just "The Wumpus caught and ate you."
            else Just "The Wumpus ran away from you."
        oldWumpusPos = wumpusPosition (wumpusState gameState)
        finder = lastPosFinder gameState
        prevWumpusPos = finder oldWumpusPos
        moverInMap = mover gameState 
        -- can move anyways bc game is over
        newWumpusPos = moverInMap oldWumpusPos prevWumpusPos randomMove
    in
    (
    message,
    gameState {
        wumpusState=WumpusState {
            wumpusPosition=newWumpusPos
            },
        randomGen=newestGen,
        isAlive=not doKill
        }
    ) 

handleHazards :: GameState -> (Maybe String, GameState)
handleHazards gameState = 
    let 
        currentPos = currentPosition (playerState gameState)
        wumpusPos = wumpusPosition(wumpusState gameState)
    in
    if currentPos == wumpusPos 
        then handleWumpusInMyNoNoSquare gameState
        else handleEnvironmentHazards gameState
