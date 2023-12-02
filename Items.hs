module Items where
import GameState
import Rooms
import Data.List

-- Function to inspect an item in the current room
inspectItem :: String -> GameState -> GameState
inspectItem itemName gamestate =
  case find (\item -> item == itemName) (roomItems (currentRoom gamestate)) of
    Just _ -> gamestate { currentRoom = (currentRoom gamestate) }
    Nothing -> gamestate { currentRoom = (currentRoom gamestate) }

-- Function to take an item from the current room and add it to the inventory
takeItem :: String -> GameState -> GameState
takeItem itemName gamestate =
  case find (\item -> item == itemName) (roomItems (currentRoom gamestate)) of
    Just _ -> gamestate { currentRoom = (currentRoom gamestate) { roomItems = filter (/= itemName) (roomItems (currentRoom gamestate)) }
                        , inventory = itemName : (inventory gamestate)
                        }
    Nothing -> gamestate { currentRoom = (currentRoom gamestate) }