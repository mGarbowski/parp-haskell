import Data.List
import Rooms
import GameState
import Items
import Interactables
import Computer
import Commands.Instructions
import Commands.Unlock
import Commands.Movement
import Commands.Inventory
import Commands.Hint
import Commands.Inspect
import Commands.EnterVent
import Commands.EnterKeycode
import qualified Data.Map as Map

-- Function to process player input and update game state
processInput :: String -> GameState -> IO GameState
processInput input gamestate
  | "look" `isPrefixOf` input          = return gamestate { currentRoom = (currentRoom gamestate) }
  | "inspect" `isPrefixOf` input       = handleInspect (drop 8 input) gamestate
  | "take" `isPrefixOf` input          = takeItem (drop 5 input) gamestate
  | "inventory" `isPrefixOf` input     = displayInventory gamestate
  | "instructions" `isPrefixOf` input  = displayInstructions >> return gamestate
  | "hint here" == input               = displayHintForCurrentRoom gamestate
  | "hint" `isPrefixOf` input          = displayHintForInteractable (drop 5 input) gamestate
  | "go" `isPrefixOf` input            = moveDirection (drop 3 input) gamestate
  | "unlock" `isPrefixOf` input        = handleUnlock (drop 7 input) gamestate
  | "enter vent" == input              = tryEnterVent gamestate
  | "enter keypad" `isPrefixOf` input  = tryEnterKeycode (drop 13 input) gamestate
  | "computer" `isPrefixOf` input      = runComputer >> return gamestate -- todo remove this
  | otherwise = putStrLn "Invalid command. Type 'instructions' to see available commands." >> return gamestate
  -- todo open, power on, put on

-- Function to run the game loop
gameLoop :: GameState -> IO ()
gameLoop gamestate = do
  putStrLn $ displayRoom (currentRoom gamestate)
  putStrLn "Enter your command:"
  putStr "> "
  input <- getLine
  putStrLn ""
  newGameState <- processInput input gamestate
  gameLoop newGameState

-- Main function to start the game
main :: IO ()
main = do
  putStrLn "Welcome to the Text Adventure Game!"
  displayInstructions
  gameLoop initialGameState
