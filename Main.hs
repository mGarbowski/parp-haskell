import Data.List
import Instructions
import Rooms
import GameState
import Movement
import Items
import Interactables
import Computer
import qualified Data.Map as Map

-- Function to process player input and update game state
processInput :: String -> GameState -> IO GameState
processInput input gamestate
  | "look" `isPrefixOf` input = return gamestate { currentRoom = (currentRoom gamestate) }
--  | "inspect" `isPrefixOf` input = inspectItem (drop 8 input) gamestate
  | "take" `isPrefixOf` input = takeItem (drop 5 input) gamestate
  | "inventory" `isPrefixOf` input = displayInventory gamestate
  | "instructions" `isPrefixOf` input = do
      displayInstructions
      return gamestate
  | "hint here" == input = displayHintForCurrentRoom gamestate
  | "hint" `isPrefixOf` input = displayHintForInteractable (drop 5 input) gamestate
  | "go" `isPrefixOf` input = moveDirection (drop 3 input) gamestate
  | "computer" `isPrefixOf` input = runComputer >> return gamestate -- todo remove this
  | otherwise = putStrLn "Invalid command. Type 'instructions' to see available commands." >> return gamestate
  -- todo open, unlock, enter, power on, put on

-- TODO move to appropriate file
displayHintForCurrentRoom :: GameState -> IO GameState
displayHintForCurrentRoom gameState =
  let room = currentRoom gameState
      hintText = roomHint room
  in putStrLn hintText >> return gameState

displayHintForInteractable :: String -> GameState -> IO GameState
displayHintForInteractable entityName gameState = do
  let inventoryItems = map (\(i, c) -> i) (Map.elems (inventory gameState))
      itemsInRoom = roomItems $ currentRoom gameState
      roomInteractables = interactables $ currentRoom gameState
      allInteractables = inventoryItems ++ itemsInRoom ++ roomInteractables

  case filter (\i -> name i == entityName) allInteractables of
    [entity] -> putStrLn (hint entity)
    _ -> putStrLn "I don't see it here"

  return gameState

---- Function to inspect an item in the current room
--inspectItem :: String -> GameState -> IO GameState
--inspectItem itemName gamestate =
--  let itemsInRoom = roomItems (currentRoom gamestate)
--      inventoryItems = inventory gamestate
--      inspectableItems = itemsInRoom ++ inventoryItems
--  in case find (\item -> name item == itemName) inspectableItems of
--    Just item -> (putStrLn $ description item) >> return gamestate
--    Nothing -> putStrLn "I don't see that here" >> return gamestate
--

displayInventory :: GameState -> IO GameState
displayInventory gameState =
  let pairs = Map.elems (inventory gameState)
      lines = map (\(item, count) -> "- " ++ name item ++ " x" ++ show count) pairs
      text = intercalate "\n" lines
  in putStrLn text >> return gameState

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
