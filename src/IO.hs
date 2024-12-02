module IO where
import Types (Move(..))
import Types (Sense(..))
import Types (Choice(..))
-- enter a room, do an action (sense shoot or nothing), then you get prompted to move
-- arrow movement needs to be implemented


-- enter a room, you get the option to sense, shoot, or move
    -- needs to be a function that will start the right chaining of input


-- Function to get the user's move with validation
getMoveFromUser :: IO Move
getMoveFromUser = do
    output "Enter your move (1 for Left, 2 for Right, 3 for Back): "
    move <- getLine  -- Read user input
    case playerMoveToGame move of
        Just validMove -> do
            output $ "You selected: " ++ show validMove
            return validMove
        Nothing -> do
            output "Invalid Option: Please try again."
            getMoveFromUser  -- Recursively call until valid input is received

-- Function to convert user input string to the Move type
playerMoveToGame :: String -> Maybe Move
playerMoveToGame move = case move of
    "1" -> Just Types.Left
    "2" -> Just Types.Right
    "3" -> Just Types.Back
    _   -> Nothing

-- have the player input the direction of the arrow move
getArrowMoveFromUser :: IO Move
getArrowMoveFromUser = do
    output "Enter your move (1 for Left, 2 for Right): "
    move <- getLine  -- Read user input
    case playerMoveToGame move of
        Just validMove -> do
            output $ "You selected: " ++ show validMove
            return validMove
        Nothing -> do
            output "Invalid Option: Please try again."
            getMoveFromUser  -- Recursively call until valid input is received

-- Function to convert user input string for arrow to the Move type
arrowMoveToGame :: String -> Maybe Move
arrowMoveToGame move = case move of
    "1" -> Just Types.Left
    "2" -> Just Types.Right
    _   -> Nothing


-- have the player input the direction of the arrow move
getSenseFromUser :: IO Sense
getSenseFromUser = do
    output "Enter what sense you use (1 for Hear, 2 for Feel, 3 for Smell): "
    sense <- getLine  -- Read user input
    case senseToGame sense of
        Just validSense -> do
            output $ "You selected: " ++ show validSense
            return validSense
        Nothing -> do
            output "Invalid Option: Please try again."
            getSenseFromUser  -- Recursively call until valid input is received

senseToGame :: String -> Maybe Sense
senseToGame sense = case sense of
    "1" -> Just Types.Hear
    "2" -> Just Types.Feel
    "3" -> Just Types.Smell
    _   -> Nothing

-- Have the player choose if they want to move, sense, or shoot when they first enter the room
getChoiceFromUser :: IO Choice
getChoiceFromUser = do
    output "Enter what choice you want (1 for Move, 2 for Sense, 3 for Shoot): "
    choice <- getLine  -- Read user input
    case choiceToGame choice of
        Just validChoice -> do
            --output $ "You selected: " ++ show validChoice
            return validChoice
        Nothing -> do
            output "Invalid Option: Please try again."
            getChoiceFromUser  -- Recursively call until valid input is received

-- translate the user's choice to what type is going to be used
choiceToGame :: String -> Maybe Choice
choiceToGame choice = case choice of
    "1" -> Just Types.ChoiceMove
    "2" -> Just Types.ChoiceSense
    "3" -> Just Types.ChoiceShoot
    _   -> Nothing


output :: String -> IO ()
output = putStrLn

-- if the arrow ends up in the room with the wumpus, it kills the wumpus 
-- otherwise the wumpus gets scared and moves rooms (it cannot be killed)
