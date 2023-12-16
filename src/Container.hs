module Container where
import qualified Data.Map as Map
import Data.List
import Interactables
import Items
import GameState
import Data.Maybe (fromJust)
import Rooms
import Util

takeItemFromContainer :: String -> GameState -> IO GameState
-- handles removal of an item from its container
takeItemFromContainer itemName gameState =
  case isContainerAvaialble itemName gameState of
    False -> putStrLn "It's not in your reach. Perhaps you need to be closer to it?" >> return gameState
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
itemNameToContainer = Map.fromList [((name coat), (name locker)),
                                    ((name powerCell), (name toolChest)),
                                    ((name crowbar), (name toolChest)),
                                    ((name labShoes), (name compartment))]


getContainerName :: String -> String
-- returns the name of the container bound to the given item name
getContainerName itemName =
  fromJust $ Map.lookup itemName itemNameToContainer

getContainerContents :: String -> GameState -> [Interactable]
-- returns contents of the container related to the given itemName
getContainerContents itemName gameState =
  fromJust $ Map.lookup (getContainerName itemName) (containerContents gameState)

canRemoveFromContainer :: String -> GameState -> Bool
-- returns whether the item can be removed from a container or it has already been removed before
canRemoveFromContainer itemName gameState =
  itemName `elem` (map name (getContainerContents itemName gameState))

isContainerAvaialble :: String -> GameState -> Bool
{- if the container related to the itemName is in the same room as the player, returns True. Else False
    Related to the experiment room case when toolChest is not interactable until the player has the boots on -}
isContainerAvaialble itemName gameState =
  let currentRoomInteractables = interactables $ currentRoom gameState
      relatedContainer = getContainerName itemName
      currRoomInteractablesByName = map name currentRoomInteractables
  in relatedContainer `elem` currRoomInteractablesByName
