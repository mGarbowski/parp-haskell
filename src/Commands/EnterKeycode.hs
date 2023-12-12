module Commands.EnterKeycode where
import GameState

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