module GameState where
import Rooms
import Interactables
import qualified Data.Map as Map

data GameState = GameState {
  currentRoom :: Room,
  inventory :: Map.Map String (Interactable, Int),  -- mapping item name to item object and count
  roomStates :: Map.Map String Room,
  keycodeEntered :: Bool,
  ventBlocked :: Bool,
  generatorOn :: Bool,
  elevatorOn :: Bool
}

-- Function to initialize the game state
initialGameState :: GameState
initialGameState = GameState {
  currentRoom = lockerRoom,
  inventory = Map.empty,
  roomStates = Map.fromList [
    (roomName lockerRoom, lockerRoom),
    (roomName corridorOne, corridorOne),
    (roomName corridorTwo, corridorTwo),
    (roomName securityRoom, securityRoom),
    (roomName experimentRoom, experimentRoom),
    (roomName generatorRoom, generatorRoom),
    (roomName computerRoom, computerRoom),
    (roomName vent, vent),
    (roomName exitRoom, exitRoom)
  ],
  keycodeEntered = False,
  ventBlocked = True,
  generatorOn = False,
  elevatorOn = False
}

-- Helper function returning list of all objects that the player can interact with
allInteractables :: GameState -> [Interactable]
allInteractables gameState =
  let inventoryItems = map fst (Map.elems (inventory gameState))
      itemsInRoom = roomItems $ currentRoom gameState
      roomInteractables = interactables $ currentRoom gameState
  in inventoryItems ++ itemsInRoom ++ roomInteractables