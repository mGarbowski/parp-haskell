module Commands.PowerOn where
import qualified Data.Map as Map
import Computer
import GameState
import Rooms
import Items
import Interactables

-- Turn on the computer in computer room
tryPowerOnComputer :: GameState -> IO GameState
tryPowerOnComputer gameState =
  if roomName (currentRoom gameState) == roomName computerRoom
  then runComputer >> return gameState
  else putStrLn "I don't see that here" >> return gameState

-- Turn on the generator if the player has enough power cells
tryPowerOnGenerator :: GameState -> IO GameState
tryPowerOnGenerator gameState =
  case roomName (currentRoom gameState) == roomName generatorRoom of
    False -> putStrLn "I don't see that here" >> return gameState
    True -> case generatorOn gameState of
      True -> putStrLn "The air vibrates with renewed energy, and the machinery's gentle rumble echoes through the entire lab." >> return gameState
      False -> case powerCellCount gameState of
        2 -> do
          putStrLn "With a satisfying click, you slot the power cells into place."
          putStrLn "The generator springs to life with a reassuring hum. Lights flicker on, flooding the room with a steady glow."
          putStrLn "The air vibrates with renewed energy, and the machinery's gentle rumble signals success, powering up the entire lab."
          return gameState {generatorOn = True}
        _ -> putStrLn "Power remains elusive without the necessary cells. Find and install them to awaken this dormant machine." >> return gameState


powerCellCount :: GameState -> Int
powerCellCount gameState =
  case Map.lookup (name powerCell) (inventory gameState) of
    Nothing -> 0
    Just (_, count) -> count


-- Power on the elevator using control panel in Security Room
tryPowerOnElevator :: GameState -> IO GameState
tryPowerOnElevator gameState =
  case roomName (currentRoom gameState) == roomName securityRoom of
    False -> putStrLn "How?" >> return gameState
    True -> case elevatorOn gameState of
      True -> putStrLn "It is already powered, it really won't work any better if you spam this button..." >> return gameState -- todo add the easter egg
      False -> case generatorOn gameState of
        False -> putStrLn "You hear an unpleasant <buzz>, looks like the backup power supply system is not enough to power the elevator. It needs more juice!" >> return gameState
        True -> putStrLn "You hear a friendly <click> sound and see a reassuring flash of green light" >> return gameState {elevatorOn = True}