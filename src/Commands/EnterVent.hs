module Commands.EnterVent where
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
