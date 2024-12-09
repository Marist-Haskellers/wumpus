module Hazards where
import Types (Hazard(..), Sense(..), Position, CaveLayout, Move(..), GameState(..), PlayerState(..), WumpusState(..), EnvironmentState(..), Move)
import System.Random

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


handleEnvironmentHazards :: GameState -> (Maybe String, GameState)
handleEnvironmentHazards = undefined

handleHazards :: GameState -> (Maybe String, GameState)
handleHazards gameState = do
    let currentPos = currentPosition (playerState gameState)
    let wumpusPos = wumpusPosition(wumpusState gameState)
    if currentPos == wumpusPos 
        then 
            let 
                gen = randomGen gameState
                (doKill, newGen) = random gen :: (Bool, StdGen)
                -- TODO this stopped compiling for some reason
                -- (randomMove, newestGen) = random newGen :: (Move, StdGen)
                (randomMove, newestGen) = undefined
                message = if doKill 
                    then Just "The Wumpus caught and devoured you" 
                    else Just "The wumpus ran away from you"
                oldWumpusPos = wumpusPosition (wumpusState gameState)
                -- any valid last pos works
                -- TODO
                someLastPos = undefined
                moverInMap = mover gameState 
                newWumpusPos = moverInMap oldWumpusPos someLastPos randomMove
            in
            (
            message,
            gameState {
                wumpusState=WumpusState {wumpusPosition=newWumpusPos},
                randomGen=newestGen
                }
            ) 
        else handleEnvironmentHazards gameState
