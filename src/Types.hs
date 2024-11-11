type Arrows = Int
type Position = Int
type RoomNumber = Int
type AdjacentRooms = [RoomNumber]



data Room = Room {
    roomNumber :: Int,
    adjacentRooms :: [Int]
}

data GameState = GameSate {
    playerPos :: Position,
    wumpusPos :: Position,
    lastPos :: Position,
    cavePos :: Position,
    arrowCount :: Int
}



data Player = Player Arrows RoomNumber
data Wumpus = Wumpus RoomNumber
data Map = Map [Room]

instance Show Room where
    show (Room roomNumber adjacentRooms) = "Room: " ++ show roomNumber ++ ", Adjacent Rooms: " ++ show adjacentRooms



r1 = Room 1 [2,5,8]
r2 = Room 2 [1,3,10]
r3 = Room 3 [2,4,12]
r4 = Room 4 [3,5,14]
r5 = Room 5 [1,4,6]
r6 = Room 6 [5,7,15]
r7 = Room 7 [6,8,17]
r8 = Room 8 [1,7,11]
r9 = Room 9 [10,12,19]
r10 = Room 10 [2,9,11]
r11 = Room 11 [8,10,20]
r12 = Room 12 [3,9,13]
r13 = Room 13 [12,14,18]
r14 = Room 14 [4,13,15]
r15 = Room 15 [6,14,16]
r16 = Room 16 [15,17,18]
r17 = Room 17 [7,16,20]
r18 = Room 18 [13,16,19]
r19 = Room 19 [9,18,20]
r20 = Room 20 [11,17,19]

map = Map [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20]




