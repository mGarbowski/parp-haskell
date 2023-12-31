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
import Commands.PutOn
import Commands.Look
import IntroOutro
import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline



-- Function to process player input and update game state
processInput :: String -> GameState -> IO GameState
processInput input gamestate
  | "look" `isPrefixOf` input          = handleLook gamestate
  | "l" `isPrefixOf` input             = handleLook gamestate
  | "inspect" `isPrefixOf` input       = handleInspect (drop 8 input) gamestate
  | "examine" `isPrefixOf` input       = handleInspect (drop 8 input) gamestate
  | "enter vent" == input              = tryEnterVent gamestate
  | "enter elevator" == input          = tryEnterElevator gamestate
  | "enter keypad" `isPrefixOf` input  = tryEnterKeycode (drop 13 input) gamestate
  | "e" `isPrefixOf` input             = handleInspect (drop 2 input) gamestate
  | "take" `isPrefixOf` input          = takeItem (drop 5 input) gamestate
  | "inventory" `isPrefixOf` input     = displayInventory gamestate
  | "i" `isPrefixOf` input             = displayInventory gamestate
  | "instructions" `isPrefixOf` input  = displayInstructions >> return gamestate
  | "hint here" == input               = displayHintForCurrentRoom gamestate
  | "hint" `isPrefixOf` input          = displayHintForInteractable (drop 5 input) gamestate
  | "go" `isPrefixOf` input            = moveDirection (drop 3 input) gamestate
  | "unlock" `isPrefixOf` input        = handleUnlock (drop 7 input) gamestate
  | "power on computer" == input       = tryPowerOnComputer gamestate
  | "power on generator" == input      = tryPowerOnGenerator gamestate
  | "power on elevator" == input       = tryPowerOnElevator gamestate
  | "put on lab shoes" == input        = handlePutOnShoes gamestate
  | "clear" == input                   = putStrLn "\ESC[2J" >> return gamestate
  | otherwise = putStrLn "Invalid command. Type 'instructions' to see available commands." >> return gamestate


-- Function to run the game loop
gameLoop :: GameState -> IO ()
gameLoop gamestate = if gameOver gamestate
  then return()
  else do
    runInputT defaultSettings $ do
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
  displayInstructions
  displayIntro
  putStrLn $ roomDescription (currentRoom initialGameState)
  putStrLn "Enter your command:"
  gameLoop initialGameState
