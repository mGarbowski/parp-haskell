import Data.List
import Instructions
import Rooms
import GameState
import Movement
import Items
import qualified Data.Map as Map

-- Function to process player input and update game state
processInput :: String -> GameState -> IO GameState
processInput input gamestate
  | "look" `isPrefixOf` input = return gamestate { currentRoom = (currentRoom gamestate) }
  | "inspect" `isPrefixOf` input = inspectItem (drop 8 input) gamestate
  | "take" `isPrefixOf` input = takeItem (drop 5 input) gamestate
  | "inventory" `isPrefixOf` input = displayInventory gamestate
  | "instructions" `isPrefixOf` input = do
      displayInstructions
      return gamestate
  | "hint" `isPrefixOf` input = do  -- todo also handle hints for items
      putStrLn $ roomHint $ currentRoom gamestate
      return gamestate
  | "go" `isPrefixOf` input = moveDirection (drop 3 input) gamestate
  | otherwise = putStrLn "Invalid command. Type 'instructions' to see available commands." >> return gamestate
  -- todo open, unlock, enter, power on, put on

-- TODO move to appropriate file
-- Function to inspect an item in the current room
inspectItem :: String -> GameState -> IO GameState
inspectItem itemName gamestate =
  let itemsInRoom = roomItems (currentRoom gamestate)
      inventoryItems = inventory gamestate
      inspectableItems = itemsInRoom ++ inventoryItems
  in case find (\item -> name item == itemName) inspectableItems of
    Just item -> (putStrLn $ description item) >> return gamestate
    Nothing -> putStrLn "I don't see that here" >> return gamestate

-- Function to take an item from the current room and add it to the inventory
takeItem :: String -> GameState -> IO GameState
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

displayInventory :: GameState -> IO GameState
displayInventory gameState =
  let items = inventory gameState
      entries = map (\i -> name i ++ " x" ++ (show $ count i)) items
      text = intercalate "\n" entries
  in putStrLn text >> return gameState

-- helper functions
addItemToInventory :: Item -> [Item] -> [Item]
addItemToInventory newItem inventory =
  case find (\existingItem -> name existingItem == name newItem) inventory of
    Just _ -> map (\item ->
      if name item == name newItem
      then updateItemCount item (count newItem)
      else item)
      inventory
    Nothing -> newItem : inventory

-- Helper function to increase count for an existing item
updateItemCount :: Item -> Int -> Item
updateItemCount item val = item { count = count item + val }

removeItemFromRoom :: Item -> Room -> Room
removeItemFromRoom item room =
  let prevItems = roomItems room
      newItems = filter (\i -> name i /= name item) prevItems
  in room {roomItems = newItems}


-- Function to run the game loop
gameLoop :: GameState -> IO ()
gameLoop gamestate = do
  putStrLn $ displayRoom (currentRoom gamestate)
  putStrLn "Enter your command:"
  putStr "> "
  input <- getLine
  putStrLn ""
  newGameState <- processInput input gamestate
  gameLoop newGameState

-- Main function to start the game
main :: IO ()
main = do
  putStrLn "Welcome to the Text Adventure Game!"
  displayInstructions
  gameLoop initialGameState
