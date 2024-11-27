module Types where

type Position = Int 
type Previous = Position
type Current = Position

-- if move is an enum then it forces the game to have
--    # of enums amount of connections per cave
--    a more generic way of representing moves could be
--    used but it may result it move names which are bland (e.i. to room 3)
--    or move names which are incorrect (e.i. moving left when in that postion you can only move right)
-- For a decahedron it Left Right Back make sense for every move as you will always have those options
--    if orientated correctly
data Move = Left | Right | Back deriving (Show)

data Sense = Smell | Hear deriving (Show)

data PlayerState = Player
  { playerPosition :: Position,
    -- will have to be set to the correct position on game start
    --    to orientate player
    lastPostion :: Position,
    playerArrowCount :: Int
  } deriving (Show)


formatPlayerState :: PlayerState -> String
formatPlayerState (Player current last arrows) =
  "--------------------------------\n" ++ 
  "Current Cave: " ++ show current ++ "\n" ++
  "   Last Cave: " ++ show last ++ "\n\n" ++
  "--------------\n" ++
  "      Arrows: " ++ show arrows ++ "\n" ++
  "--------------------------------"

formatPlayerStateBetter :: PlayerState -> String
formatPlayerStateBetter (Player current last arrows) =
  "                     ________________ |" ++ "\n" ++
  "                    | Status : " ++ "\n" ++
  "                    |--------       " ++ "\n" ++
  "                    | Current Cave: " ++ show current ++ "\n" ++
  "                    |    Last Cave: " ++ show last ++ "\n" ++
  "                    |           --  " ++ "\n" ++
  "                    |      Arrows:  " ++ show arrows ++ "\n" ++
  "                    |________________ "


data WumpusState = WumpusState
  { wumpusPosition :: Position
  } deriving (Show)

data EnvironmentState = EnvironmentState
  { hazards :: [(Position, Hazard)]
  } deriving (Show)


data GameState = GameState
  { playerState :: PlayerState,
    wumpusState :: WumpusState,
    environment :: EnvironmentState,
    layout      :: CaveLayout
  } deriving (Show)

type RoomId = Int

data RoomFeature = Wumpus | Bat | Breeze | Empty
  deriving (Show, Eq)

data Room = Room
  { roomId      :: RoomId
  , roomFeature :: RoomFeature
  } deriving (Show, Eq)




data Hazard = Bats | Pit deriving (Show)

type CaveLayout = [(Position, [Position])] 

-- Map Layout:
decahedron :: CaveLayout
decahedron =
  [ (1, [2, 5, 8, 2, 5, 8]),
    (2, [1, 3, 10, 1, 3, 10]),
    (3, [2, 4, 12, 2, 4, 12]),
    (4, [3, 5, 14, 3, 5, 14]),
    (5, [1, 4, 6, 1, 4, 6]),
    (6, [5, 7, 15, 5, 7, 15]),
    (7, [6, 17, 8, 6, 17, 8]), -- originally wrong
    (8, [1, 7, 9, 1, 7, 9]),
    (9, [8, 18, 10, 8, 18, 10]), -- originally wrong
    (10, [2, 9, 11, 2, 9, 11]),
    (11, [10, 19, 12, 10, 19, 12]), -- originally very WRONG 
    (12, [3, 11, 13, 3, 11, 13]), -- good
    (13, [12, 20, 14, 12, 20, 14]), -- originally wrong
    (14, [4, 13, 15, 4, 13, 15]), -- good
    (15, [6, 14, 16, 6, 14, 16]), -- good
    (16, [15, 20, 17, 15, 17, 20]), -- originally wrong
    (17, [7, 16, 18, 7, 16, 18]), -- good
    (18, [9, 17, 19, 9, 17, 19]),
    (19, [11, 18, 20, 11, 18, 20]),
    (20, [13, 19, 16, 13, 16, 19]) -- originally wrong
  ]



getNeighbors :: Position -> CaveLayout -> [Position]
getNeighbors current_position l = head [neighbors | (pos, neighbors) <- l, pos == current_position]

findIndexOf :: Eq a => a -> [a] -> Position
findIndexOf x xs = go xs 0
  where
    go [] _ = error "Not found"
    go (y:ys) i
      | x == y  = i
      | otherwise = go ys (i + 1)



-- e.g., getThreeIndicesStartingAtIndex 2 [2,5,8,2,5,8] => [8, 2, 5]
getThreeIndicesStartingAtIndex :: Int -> [Position] -> [Position]
getThreeIndicesStartingAtIndex index list = take 3 (drop index list) 

getOrientationAdjustedNeighbors :: Current -> Previous -> CaveLayout -> [Position] 
getOrientationAdjustedNeighbors current prev l = getThreeIndicesStartingAtIndex (findIndexOf prev (getNeighbors current l)) (getNeighbors current l)

-- takes index of the first occurrence of the player's "last position"
-- returns three indices with the last position being the first, followed by the right and the left
-- it does this by dropping up until the index of the first occurrence and returns the three init indices

-- returns list of senses, calculating what is smelled and heard in adjacent rooms
--getSenses :: [Position] -> [Sense]




menuHeader :: String
menuHeader = unlines
  [
    "--------------------------------------",
    "               Menu Options           |",
    "                                      |",
    "                                      |"
  ]


menuBody :: String
menuBody = unlines
  [
    "\n\n   Actions:                     ",
    "     - Smell                    ",
    "     - Listen                   ",
    "     - Shoot Left               ",
    "     - Shoot Right               ",
    "",
    "",
    "   Movement:",
    "     - Left",
    "     - Right",
    "",
    "",
    "--------------------------------------",
    "Enter Option: "
  ]

caveArt :: String
caveArt = unlines
  [
  "*%@%%%%%@@@@@@@@*+%@@@@@@@@@@@@%%##%%%#%%##%#%%%%%@@%%%@@@@%@@@@@%%@@@@@@@@%#"
  ,"%%%%##%%@@@@@%##*#@@@@@@@@@@@@@@%%%%%#%@%@%%%#%%%%%%%%@@@@@@@%@@@@@@@@@@%@@%%"
  ,"+*#%#*###=#%@@%%@@@%@@@@@@@@@@@@@@@@@@%%%%@%%@@@@%+=*@@@@@@@@@@@@@@@%@@@@%%#%"
  ,"+###*#@#####@@@@@@@@@@@@@@@@@@@@@@@@@%@@@@@@@@@@@@%*%@@@@@@@@@@@@@@@@@@@@%%##"
  ,"#######%%%%%%@@@@@@@@@@@@@@@@@@@@@@@%%%%%%@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@%%%%"
  ,"%%%#%%%%%%#%@@@@@@@@@@@@@%%@@@@@@@%%#%@%@@@@@@@@@@@@%@@@@@@@%##+%@@@@@@@%@@%%"
  ,"#%%%%%%%%%%%@@@@@@@@@@@@%%%%%%%%%%%%%@@@@@@@@@@@@@@@@@@@@%%*++=++=+*%@@@@@@@%"
  ,"##%%%%%%%%@@@@@%@@@@@%%%@%%#%%%%%%#%%@@@@@@@@@@@%%%%%@@@#=:.. ...-=-=+*%@@%%%"
  ,"#%@%%%%%%%@@%%%%%%%#+-:::::-*##%#%@@@@@@@@@%@%%%%%%%%#+:........  ...:-*@@@%@"
  ,"@%%#%%%@%@#+**#++=-:.........::*#%@@@@@@@@@@%#%%##%**=........        .:+%%%%"
  ,"#**+*##%@%***+=:..... .. ......-###@@@%@@@%@#*%##%**=.......            .=%%%"
  ,"%#######%%##+:         .........-#%@@@@%@@%%######*+.....                .=*+"
  ,"%%@#*%@%%%*#-.           ........+%@%@@@%@%@##%%%#*-..                    .*+"
  ,"%%%@@%@@@%#+.        ............:*@%@@@@@%%%%%##*-..                     .+#"
  ,"%%#%%@@@@@%=.         ...........:+#%@@@@@%@%@#%##:..                     .*%"
  ,"%####%@@%##-               .    ..=*%@@@@@@@%@%%@*....                    .#@"
  ,"%%%@@@@@@@*:                     .=*%@@@@@%@%@@%@=....                    -@%"
  ,"@@@@@%%%@@=.                 . . .=*%@@@@@@@@@@@@-...                    .+@%"
  ,"@@@@@@%%%%:.               .. ....-%@@@@@@@@@@@@%:::..                   .%@@"
  ,"%@@##+*%@%.               ........:*@@@@@@@@@@@@=.:...                   -@@@"
  ,"%###**##%%=.              .........=@@@@@@@@@%%+..  ..                   *@@@"
  ,"%%%%%#####*:                .....  .:*#%@@@@@#--:.                      .@@@@"
  ,"%%%%#*%%%%+:                ......::+@@@@@@@@@@#=-.                     :@@@@"
  ,"@@%%#==#%%=:.:--===----=++=--::.---%%@@@@@@@@@@@*=.                     +@@@@"
  ,"@@%+==++##-.:==++++********####*###@@@@@@@@@@@@@@+.....::::.......   ...#@@@@"
  ,"@@#+-=++*:.  .-==+++*********#####%%@@@@@@@@@@@@@%=-----==----:......:-:=@@@@"
  ,"%#**+*#%-=++*.:-=+++++******#########@@@@@@@@@@@@*+*####*****+====+*+=-=*@@@@"
  ,"@@%@@@@@+@@@@@:-==++++*******#####**#%@@@@@@@@@@%####%####*****++++====+@@@@@"
  ,"#@@@@@@@@@@@@*#***#####+++************%@@@@@@@@@######*********#**+**##%@@@@@"
  ,"%@@@@@@@@@@@@%%*####+=+++***********##%@@@@@@@@###**#******###*******##@@@@@@"
  ,"@@@@@@%%@@@@#==-=-::-+#%%%%#**********#@@@@@@@@%%##%%%%%#***********##@@@@@@@"
  ,"#%%@@@%%@@@@==+**#########*+++++++****##@@@@@#####**%###**********###@@@@@@@@"
  ,"*##%%@@@@@#+++++*##%%*+++**+**+++++++++**#%#*#####*#**##*#*#*****###%@@@@@@@@"
  ]



