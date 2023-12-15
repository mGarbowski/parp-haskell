import Data.List
import GameState
import Rooms
import Commands.Instructions
import Commands.Unlock
import Commands.Movement
import Commands.Inventory
import Commands.Hint
import Commands.Inspect
import Commands.Enter
import Commands.PowerOn
import Commands.IntroOutro
import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline



-- Function to process player input and update game state
processInput :: String -> GameState -> IO GameState
processInput input gamestate
  | "look" `isPrefixOf` input          = liftIO (putStrLn $ displayRoom (currentRoom gamestate)) >> return gamestate
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
  | "power on computer" == input       = tryPowerOnComputer gamestate
  | "power on generator" == input      = tryPowerOnGenerator gamestate
  | "power on elevator" == input       = tryPowerOnElevator gamestate
  | "clear" == input                   = putStrLn "\ESC[2J" >> return gamestate
  | otherwise = putStrLn "Invalid command. Type 'instructions' to see available commands." >> return gamestate
  -- todo open, power on, put on, enter elevator

-- Function to run the game loop
gameLoop :: GameState -> IO ()
gameLoop gamestate = do
  runInputT defaultSettings $ do
    outputStrLn "Enter your command:"
    minput <- getInputLine "> "
    case minput of
      Nothing -> return ()
      Just input -> liftIO $ do
        putStrLn ""
        newGameState <- processInput input gamestate
        gameLoop newGameState

-- Main function to start the game
main :: IO ()
main = do
  displayIntro
  displayInstructions
  putStrLn $ displayRoom (currentRoom initialGameState)
  gameLoop initialGameState
