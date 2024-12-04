module Hazards where
import Types (Hazard(..), Sense(..), Position, CaveLayout, Move(..), GameState(..), PlayerState(..))

toSense :: Hazard -> Sense
toSense Bats = Hear
toSense Pit  = Feel


senseHazards :: GameState -> [Sense]
senseHazards gameState = do
    -- maybe there should be a better way
    let movementOptions = [Types.Left, Types.Right, Types.Back];
    let currentPos = currentPosition (playerState gameState)
    let lastPos = lastPosition (playerState gameState)
    let moveInMap = mover gameState
    let neighbors = map (\move -> moveInMap currentPos lastPos move) movementOptions
    let senses = if wumpusPosition (wumpusState gameState) `elem` neighbors then [Smell] else []
    return senses

    -- let neighbors = layout !! position
    --     nearbyHazards = filter (\(pos, _) -> pos `elem` neighbors) hazards
    -- in map (toSense . snd) nearbyHazards


handleHazards :: Position -> [(Position, Hazard)] -> Maybe String
handleHazards position hazards =
    case lookup position hazards of
        Just Bats -> Just "A swarm of super-bats swoops in and lifts you away."
        Just Pit  -> Just "You fell into a bottomless pit and died."
        Nothing   -> Nothing
