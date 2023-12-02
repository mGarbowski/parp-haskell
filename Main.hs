import Data.List

-- Define types for Room and GameState
data Room = Room { roomName :: String, roomDescription :: String, roomItems :: [String] }
data GameState = GameState { currentRoom :: Room, inventory :: [String] }

-- Function to initialize the game state
initialGameState :: GameState
initialGameState = GameState
  { currentRoom = initialRoom
  , inventory = []
  }

-- Function to define the initial room
initialRoom :: Room
initialRoom = Room
  { roomName = "Starting Room"
  , roomDescription = "You find yourself in a dimly lit room."
  , roomItems = ["Key", "Note"]
  }

-- Function to display the current room description and items
displayRoom :: Room -> String
displayRoom room = roomDescription room ++ "\nItems in the room: " ++ intercalate ", " (roomItems room)

-- Function to display available instructions
displayInstructions :: IO ()
displayInstructions = do
  putStrLn "Available Instructions:"
  putStrLn "  - look: View the current room"
  putStrLn "  - inspect X: Inspect an item in the room"
  putStrLn "  - take X: Take an item from the room and add it to your inventory"
  putStrLn "  - inventory: View your current inventory"
  putStrLn "  - instructions: View available instructions"

-- Function to process player input and update game state
processInput :: String -> GameState -> IO GameState
processInput input gamestate
  | "look" `isPrefixOf` input = return gamestate { currentRoom = (currentRoom gamestate) }
  | "inspect" `isPrefixOf` input = return $ inspectItem (drop 8 input) gamestate
  | "take" `isPrefixOf` input = return $ takeItem (drop 5 input) gamestate
  | "inventory" `isPrefixOf` input = do
      putStrLn $ "Inventory: " ++ intercalate ", " (inventory gamestate)
      return gamestate
  | "instructions" `isPrefixOf` input = displayInstructions >> return gamestate
  | otherwise = do
      putStrLn "Invalid command. Type 'instructions' to see available commands."
      return gamestate

-- Function to inspect an item in the current room
inspectItem :: String -> GameState -> GameState
inspectItem itemName gamestate =
  case find (\item -> item == itemName) (roomItems (currentRoom gamestate)) of
    Just _ -> gamestate { currentRoom = (currentRoom gamestate) }
    Nothing -> gamestate { currentRoom = (currentRoom gamestate) }

-- Function to take an item from the current room and add it to the inventory
takeItem :: String -> GameState -> GameState
takeItem itemName gamestate =
  case find (\item -> item == itemName) (roomItems (currentRoom gamestate)) of
    Just _ -> gamestate { currentRoom = (currentRoom gamestate) { roomItems = filter (/= itemName) (roomItems (currentRoom gamestate)) }
                        , inventory = itemName : (inventory gamestate)
                        }
    Nothing -> gamestate { currentRoom = (currentRoom gamestate) }

-- Function to run the game loop
gameLoop :: GameState -> IO ()
gameLoop gamestate = do
  putStrLn $ displayRoom (currentRoom gamestate)
  putStrLn "Enter your command:"
  input <- getLine
  newGameState <- processInput input gamestate
  gameLoop newGameState

-- Main function to start the game
main :: IO ()
main = do
  putStrLn "Welcome to the Text Adventure Game!"
  displayInstructions
  gameLoop initialGameState
