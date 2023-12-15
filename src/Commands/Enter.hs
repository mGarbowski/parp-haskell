module Commands.Enter where
import Data.List
import GameState
import Rooms
import Interactables

-- Enter vent if possible
tryEnterVent :: GameState -> IO GameState
tryEnterVent gameState =
  let roomInteractables = interactables (currentRoom gameState)
  in case elem ventEntrance roomInteractables of
    False -> putStrLn "I don't see that here" >> return gameState
    True -> case ventBlocked gameState of
      True -> do
        putStrLn "The vent is closed shut and the door doesn't seem to budge. Maybe using some tool would help?"
        return gameState
      False -> return gameState {currentRoom = vent}


-- Enter keycode on the keypad in locker room
tryEnterKeycode :: String -> GameState -> IO GameState

-- Keycode correct, player still has to unlock the door
tryEnterKeycode "852611" gameState = do
  putStrLn "The light on the lock blinks green, it buzzes and unlocks. Correct!"
  return gameState {keycodeEntered = True}

-- Incorrect keycode
tryEnterKeycode _ gameState = do
  putStrLn "The light on the lock blinks red. The provided code was incorrect."
  return gameState