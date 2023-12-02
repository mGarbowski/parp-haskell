import Data.List
import Instructions
import Rooms
import Items
import GameState

-- Function to process player input and update game state
processInput :: String -> GameState -> IO GameState
processInput input gamestate
  | "look" `isPrefixOf` input = return gamestate { currentRoom = (currentRoom gamestate) }
  | "inspect" `isPrefixOf` input = return $ inspectItem (drop 8 input) gamestate
  | "take" `isPrefixOf` input = return $ takeItem (drop 5 input) gamestate
  | "inventory" `isPrefixOf` input = do
      putStrLn $ "Inventory: " ++ intercalate ", " (inventory gamestate)
      return gamestate
  | "instructions" `isPrefixOf` input = do
      displayInstructions
      return gamestate
  | "hint" `isPrefixOf` input = do
      putStrLn $ roomHint $ currentRoom gamestate
      return gamestate
  | "go" `isPrefixOf` input = moveDirection (drop 3 input) gamestate
  | otherwise = do
      putStrLn "Invalid command. Type 'instructions' to see available commands."
      return gamestate


moveDirection :: String -> GameState -> IO GameState
moveDirection directionStr gameState =
  case parseDirection directionStr of
    Just direction -> case lookup direction (directions (currentRoom gameState)) of
      Just otherRoom -> return gameState {currentRoom = otherRoom}
      Nothing -> do
        putStrLn "You can't go that way"
        return gameState
    Nothing -> do
      putStrLn "You can go north, south, east or west"
      return gameState

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
