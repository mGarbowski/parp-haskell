import Data.List
import Instructions
import Rooms
import GameState
import Movement
import Items

-- Function to process player input and update game state
processInput :: String -> GameState -> IO GameState
processInput input gamestate
  | "look" `isPrefixOf` input = return gamestate { currentRoom = (currentRoom gamestate) }
  | "inspect" `isPrefixOf` input = inspectItem (drop 8 input) gamestate
  | "take" `isPrefixOf` input = takeItem (drop 5 input) gamestate
  | "inventory" `isPrefixOf` input = do
      putStrLn $ "Inventory: " ++ intercalate ", " (map itemName (inventory gamestate))
      return gamestate
  | "instructions" `isPrefixOf` input = do
      displayInstructions
      return gamestate
  | "hint" `isPrefixOf` input = do
      putStrLn $ roomHint $ currentRoom gamestate
      return gamestate
  | "go" `isPrefixOf` input = moveDirection (drop 3 input) gamestate
  | otherwise = putStrLn "Invalid command. Type 'instructions' to see available commands." >> return gamestate

-- TODO move to appropriate file
-- Function to inspect an item in the current room
inspectItem :: String -> GameState -> IO GameState
inspectItem name gamestate =
  case find (\item -> itemName item == name) (roomItems (currentRoom gamestate)) of
    Just item -> (putStrLn $ itemDescription item) >> return gamestate
    Nothing -> putStrLn "I don't see that here" >> return gamestate

-- Function to take an item from the current room and add it to the inventory
takeItem :: String -> GameState -> IO GameState
takeItem name gameState =
  case find (\item -> itemName item == name) (roomItems (currentRoom gameState)) of
    Just item -> return gameState {
      currentRoom = (currentRoom gameState) {
        roomItems = filter (\item -> itemName item /= name) (roomItems (currentRoom gameState))
        },
      inventory = item : (inventory gameState)
      }
    Nothing -> putStrLn "I don't see that here" >> return gameState

-- Function to run the game loop
gameLoop :: GameState -> IO ()
gameLoop gamestate = do
  putStrLn $ displayRoom (currentRoom gamestate)
  putStrLn "Enter your command:"
  putStr "> "
  input <- getLine
  newGameState <- processInput input gamestate
  gameLoop newGameState

-- Main function to start the game
main :: IO ()
main = do
  putStrLn "Welcome to the Text Adventure Game!"
  displayInstructions
  gameLoop initialGameState
