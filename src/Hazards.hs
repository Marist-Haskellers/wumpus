module Hazards where
import Types (Hazard(..), Sense(..), Position, CaveLayout, Move(..), GameState(..), PlayerState(..), WumpusState(..), EnvironmentState(..))

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



handleHazards :: GameState -> Maybe String
handleHazards gaState =
    let currentPos = currentPosition (playerState gameState)
        hzrds = hazards (environmentState gameState)
        wumpusPos = wumpusPosition(wumpusState gameState)
currentPos == wumpusPos -> Just "The Wumpus caught and devoured you"
Just Bats <- lookup currentPos hzrds -> 
Just Pit <- lookup currentPos hzrds -> Just ""