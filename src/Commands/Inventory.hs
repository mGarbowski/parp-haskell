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
takeItemFromContainer :: String -> GameState -> IO GameState

-- special cases for container items first
takeItemFromContainer "lab shoes" gameState =
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

takeItemFromContainer "crowbar" gameState =
    if roomName (currentRoom gameState) /= roomName experimentRoom
    then putStrLn "I don't see that here" >> return gameState
    else do
        let currentInventory = inventory gameState
        let updatedInventory = addItemToInventory labShoes currentInventory
        let currentToolChestContents = toolChestContents gameState
        let updatedToolChestContents = filter (\item -> name item /= "crowbar") currentToolChestContents
        return gameState {
            inventory = updatedInventory,
            toolChestContents = updatedToolChestContents
        }

takeItemFromContainer "power cell" gameState =
    if roomName (currentRoom gameState) /= roomName experimentRoom
    then putStrLn "I don't see that here" >> return gameState
    else do
        let currentInventory = inventory gameState
        let updatedInventory = addItemToInventory labShoes currentInventory
        let currentToolChestContents = toolChestContents gameState
        let updatedToolChestContents = filter (\item -> name item /= "power cell") currentToolChestContents
        return gameState {
            inventory = updatedInventory,
            toolChestContents = updatedToolChestContents
        }

takeItemFromContainer "coat" gameState =
    if roomName (currentRoom gameState) /= roomName lockerRoom
    then putStrLn "I don't see that here" >> return gameState
    else do
        let currentInventory = inventory gameState
        let updatedInventory = addItemToInventory coat currentInventory
        return gameState {
            inventory = updatedInventory,
            lockerContents = []
        }

takeItemFromContainer _ gameState = return gameState

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
  -- if the item is in a container, call a designated function
  let alwaysInContainer = itemName `elem` ["lab shoes", "crowbar", "coat"]
      powerCellFromContainer = itemName == "power cell" && roomName (currentRoom gameState) == "Experiment Room" in
  if alwaysInContainer || powerCellFromContainer
  then takeItemFromContainer itemName gameState
  else takeItemFromRoom itemName gameState


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

-- helper functions and map binding item name to container name
itemNameToContainer :: Map.Map String String
itemNameToContainer = Map.fromList [(name coat, name locker),
                                    (name powerCell, name toolChest),
                                    (name crowbar, name toolChest),
                                    (name labShoes, name compartment)]


getContainerName :: String -> String
-- returns the name of the container bound to the given item name
getContainerName itemName =
  case Map.lookup itemName itemNameToContainer of
    Nothing -> return "placeholder"
    Just containerName -> return containerName

canRemoveFromContainer :: String -> GameState -> Bool

getContainerContents :: String -> GameState -> [Interactable]

isContainerAvaialble :: String -> GameState -> Bool