module GameState where
import Rooms
import Interactables
import Items
import qualified Data.Map as Map

data GameState = GameState {
  currentRoom :: Room,
  inventory :: Map.Map String (Interactable, Int),  -- mapping item name to item object and count
  roomStates :: Map.Map String Room,
  lockerCompartmentBlocked :: Bool,
  containerContents :: Map.Map String [Interactable],
  keycodeEntered :: Bool,
  ventBlocked :: Bool,
  generatorOn :: Bool,
  elevatorOn :: Bool,
  gameOver :: Bool
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
  lockerCompartmentBlocked = True,
  containerContents = Map.fromList [(name locker, [coat]),
                                 (name toolChest, [powerCell, crowbar]),
                                 (name compartment, [labShoes])],
  keycodeEntered = False,
  ventBlocked = True,
  generatorOn = False,
  elevatorOn = False,
  gameOver = False
}

-- Helper function returning list of all objects that the player can interact with
allInteractables :: GameState -> [Interactable]
allInteractables gameState =
  let inventoryItems = map fst (Map.elems (inventory gameState))
      itemsInRoom = roomItems $ currentRoom gameState
      roomInteractables = interactables $ currentRoom gameState
      containerItems = [coat, powerCell, crowbar, lockerRoomKey]
  in inventoryItems ++ itemsInRoom ++ roomInteractables ++ containerItems


updateCurrentRoom :: GameState -> GameState
updateCurrentRoom gameState =
  let currRoomName = roomName (currentRoom gameState)
      updatedCurrentRoom = Map.lookup currRoomName (roomStates gameState)
  in case updatedCurrentRoom of
    Nothing -> gameState
    Just room -> gameState {currentRoom = room}