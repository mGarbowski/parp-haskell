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
  | otherwise = do
      putStrLn "Invalid command. Type 'instructions' to see available commands."
      return gamestate


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
