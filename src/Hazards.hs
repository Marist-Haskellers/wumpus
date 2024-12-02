toSense :: Hazard -> Sense
toSense Bats = Hear
toSense Pit  = Feel


senseHazards :: Position -> CaveLayout -> [(Position, Hazard)] -> [Sense]
senseHazards position layout hazards =
    let neighbors = layout !! position
        nearbyHazards = filter (\(pos, _) -> pos `elem` neighbors) hazards
    in map (toSense . snd) nearbyHazards


handleHazards :: Position -> [(Position, Hazard)] -> Maybe String
handleHazards position hazards =
    case lookup position hazards of
        Just Bats -> Just "A swarm of super-bats swoops in and lifts you away."
        Just Pit  -> Just "You fell into a bottomless pit and died."
        Nothing   -> Nothing
