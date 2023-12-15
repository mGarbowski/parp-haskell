module Commands.Inventory where
import qualified Data.Map as Map
import Data.List
import GameState
import Rooms
import Items
import Interactables

displayInventory :: GameState -> IO GameState
displayInventory gameState =
  let pairs = Map.elems (inventory gameState)
      lines = map (\(item, count) -> "- " ++ name item ++ " x" ++ show count) pairs
      text = intercalate "\n" lines
  in putStrLn text >> return gameState

-- Function to take an item from the current room and add it to the inventory
takeItem :: String -> GameState -> IO GameState
takeItem "labShoes" gameState =
    if roomName (currentRoom gameState) /= roomName lockerRoom
    then putStrLn "I don't see that here" >> return gameState
    else
      case (lockerCompartmentBlocked gameState) of
      True -> putStrLn "Unlock the compartment first!" >> return gameState
      False -> do
        let currentInventory = inventory gameState
        let updatedInventory = addItemToInventory labShoes currentInventory
        return gameState {
            lockerCompartmentContents=[]
        }


takeItem itemName gameState =
  case find (\item -> name item == itemName) (roomItems (currentRoom gameState)) of
    Just item -> do
      let currentInventory = inventory gameState
      let updatedInventory = addItemToInventory item currentInventory
      let currentRoomStates = roomStates gameState
      let currentRoomName = roomName $ currentRoom gameState
      let updatedRoom = removeItemFromRoom item (currentRoom gameState)
      let updatedRoomStates = Map.insert currentRoomName updatedRoom currentRoomStates
      return gameState {
        currentRoom = updatedRoom,
        inventory = updatedInventory,
        roomStates = updatedRoomStates
      }
    Nothing -> putStrLn "I don't see that here" >> return gameState


-- helper functions
addItemToInventory :: Interactable -> Map.Map String (Interactable, Int) -> Map.Map String (Interactable, Int)
addItemToInventory newItem inventory =
  case Map.lookup (name newItem) inventory of
    Just (i, count) -> Map.insert (name newItem) (newItem, count+1) inventory
    Nothing -> Map.insert (name newItem) (newItem, 1) inventory


removeItemFromRoom :: Interactable -> Room -> Room
removeItemFromRoom item room =
  let prevItems = roomItems room
      newItems = filter (\i -> name i /= name item) prevItems
  in room {roomItems = newItems}
