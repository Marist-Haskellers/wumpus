module Types where

--data synonyms
type ArrowCount = Int
type RoomNumber = Int
type Alive = Bool
type AdjacentRooms = [RoomNumber] --added to keep track of rooms




--data types
data Sense = Smell | Hear | Feel
data GameObject = Wumpus | Bats | Pit

data Room = Room{
    roomEvent :: GameObject
    roomSense :: Sense
    roomsNear :: AdjacentRooms

    -- need to keep track of the orientation of the other rooms
}

data Player = Player{
    status :: Alive
    arrowInventory :: ArrowCount
    playerLocation :: RoomNumber
    
}

{- Actions:
Player needs to be able to move rooms (The orientation of rooms change along with room change)
Player needs to be able to shoot arrows

Wumpus needs to be able to flee or attack

Bats pick up player if in their room

Bottomless pits kills the player making Alive = False

result of the threat - transformation of the game, think of it more as a function
    
-}

