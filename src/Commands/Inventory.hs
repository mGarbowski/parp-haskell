module Commands.Inventory where
import qualified Data.Map as Map
import Data.List
import GameState
import Rooms
import Items
import Interactables
import Util
import Container

displayInventory :: GameState -> IO GameState
displayInventory gameState =
  let pairs = Map.elems (inventory gameState)
      lines = map (\(item, count) -> "- " ++ name item ++ " x" ++ show count) pairs
      text = intercalate "\n" lines
  in putStrLn text >> return gameState


takeItemFromRoom :: String -> GameState -> IO GameState
takeItemFromRoom itemName gameState =
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


takeItem :: String -> GameState -> IO GameState
takeItem itemName gameState =
  -- separate cases for removing from room or from container
  let alwaysInContainer = itemName `elem` [name labShoes, name crowbar, name coat]
      powerCellFromContainer = itemName == name powerCell && roomName (currentRoom gameState) == roomName experimentRoom in
  if alwaysInContainer || powerCellFromContainer
  then takeItemFromContainer itemName gameState
  else takeItemFromRoom itemName gameState

removeItemFromRoom :: Interactable -> Room -> Room
removeItemFromRoom item room =
  let prevItems = roomItems room
      newItems = filter (\i -> name i /= name item) prevItems
  in room {roomItems = newItems}
