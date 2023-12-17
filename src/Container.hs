module Container where
import qualified Data.Map as Map
import Data.List
import Interactables
import Items
import GameState
import Data.Maybe (fromJust)
import Rooms
import Util

-- handles removal of an item from its container
takeItemFromContainer :: String -> GameState -> IO GameState
takeItemFromContainer itemName gameState = do
  isAvailable <- (isContainerAvailable itemName gameState)
  case isAvailable of
    False -> return gameState
    True -> do
      if canRemoveFromContainer itemName gameState == False
      then putStrLn "Don't see it here" >> return gameState
      else do
        let currentInventory = inventory gameState
        let relatedContainerContents = getContainerContents itemName gameState
        let itemToAdd = fromJust $ find (\item -> name item == itemName) relatedContainerContents
        let updatedInventory = addItemToInventory itemToAdd (inventory gameState)
        let updatedContainerContents = filter (\item -> name item /= itemName) relatedContainerContents
        let currContainerMap = containerContents gameState
        let updatedContainerMap = Map.insert (getContainerName itemName) updatedContainerContents currContainerMap
        return gameState {
          inventory = updatedInventory,
          containerContents = updatedContainerMap
        }

-- helper functions and map binding item name to container name
itemNameToContainer :: Map.Map String String
itemNameToContainer = Map.fromList [
    ((name coat), (name locker)),
    ((name powerCell), (name toolChest)),
    ((name crowbar), (name toolChest)),
    ((name labShoes), (name compartment))
  ]


-- returns the name of the container bound to the given item name
getContainerName :: String -> String
getContainerName itemName =
  fromJust $ Map.lookup itemName itemNameToContainer

-- returns contents of the container related to the given itemName
getContainerContents :: String -> GameState -> [Interactable]
getContainerContents itemName gameState =
  fromJust $ Map.lookup (getContainerName itemName) (containerContents gameState)

-- returns whether the item can be removed from a container or it has already been removed before
canRemoveFromContainer :: String -> GameState -> Bool
canRemoveFromContainer itemName gameState =
  itemName `elem` (map name (getContainerContents itemName gameState))

{- if the container related to the itemName is in the same room as the player and it is not locked, returns True.
Else False. Related to the experiment room case when toolChest is not interactable until the player has the boots on
and the locked locker compartment -}
isContainerAvailable :: String -> GameState -> IO Bool
isContainerAvailable itemName gameState =
  let currentRoomInteractables = interactables $ currentRoom gameState
      relatedContainer = getContainerName itemName
      currRoomInteractablesByName = map name currentRoomInteractables
      locked = relatedContainer == name compartment && lockerCompartmentBlocked gameState in
        if locked then putStrLn "You can't take it out, the container is locked" >> return False
        else if not (relatedContainer `elem` currRoomInteractablesByName)
             then putStrLn "Don't see that here" >> return False
             else return True
