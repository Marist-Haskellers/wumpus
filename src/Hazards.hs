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



handleEnvironmentHazard :: GameState -> Maybe Hazard -> GameState
handleEnvironmentHazard gameState Nothing = gameState
handleEnvironmentHazard gameState (Just hzrd) = undefined



handleEnvironmentHazards :: GameState -> (Maybe String, GameState)
handleEnvironmentHazards gameState = do
    let hzrds = hazards (environmentState gameState)
    let currentPos = currentPosition (playerState gameState)
    -- if you could one into multiple hazards at once this would need to be changed
    let firstHazard = fmap snd (find (\(pos, _) -> pos == currentPos) hzrds)


    (Nothing, gameState)



handleHazards :: GameState -> (Maybe String, GameState)
handleHazards gameState = do
    let currentPos = currentPosition (playerState gameState)
    let wumpusPos = wumpusPosition(wumpusState gameState)
    if currentPos == wumpusPos 
        then 
            let 
                gen = randomGen gameState
                (doKill, newGen) = random gen :: (Bool, StdGen)
                (randomMove, newestGen) = random newGen :: (Move, StdGen)
                message = if doKill 
                    then Just "The Wumpus caught and ate you" 
                    else Just "The Wumpus ran away from you"
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
        else handleEnvironmentHazards gameState
