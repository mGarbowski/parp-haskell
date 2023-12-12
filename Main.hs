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
  | "look" `isPrefixOf` input          = return gamestate { currentRoom = (currentRoom gamestate) }
  | "inspect" `isPrefixOf` input       = handleInspect (drop 8 input) gamestate
  | "take" `isPrefixOf` input          = takeItem (drop 5 input) gamestate
  | "inventory" `isPrefixOf` input     = displayInventory gamestate
  | "instructions" `isPrefixOf` input  = displayInstructions >> return gamestate
  | "hint here" == input               = displayHintForCurrentRoom gamestate
  | "hint" `isPrefixOf` input          = displayHintForInteractable (drop 5 input) gamestate
  | "go" `isPrefixOf` input            = moveDirection (drop 3 input) gamestate
  | "unlock" `isPrefixOf` input        = handleUnlock (drop 7 input) gamestate
  | "computer" `isPrefixOf` input      = runComputer >> return gamestate -- todo remove this
  | otherwise = putStrLn "Invalid command. Type 'instructions' to see available commands." >> return gamestate
  -- todo open, unlock, enter, power on, put on

-- TODO move to appropriate file

handleUnlock :: String -> GameState -> IO GameState
handleUnlock directionStr gameState = -- todo check if the player has the key etc.
  case parseDirection directionStr of
    Nothing -> return gameState
    Just direction ->
      if canUnlock direction (roomName $ currentRoom gameState) gameState
      then putStrLn "Door unlocked" >> return (unlockPath direction gameState)
      else putStrLn "You can't open it" >> return gameState

-- direction, current room name, game state, can player unlock door in `direction` from current room
canUnlock :: Direction -> String -> GameState -> Bool
canUnlock East "Locker Room" gameState = Map.member (name lockerRoomKey) (inventory gameState)
canUnlock _ _ _ = False

-- update game state to make the path in given direction unlocked in both ways
unlockPath :: Direction -> GameState -> GameState
unlockPath direction gameState =
  let thisRoom = currentRoom gameState
      rooms = (roomStates gameState)
  in case Map.lookup direction (directions thisRoom) of
    Nothing -> gameState
    Just (otherRoomName, _) -> case Map.lookup otherRoomName rooms of
      Nothing -> gameState
      Just otherRoom ->
        let newThisRoom = unlockPathInRoom direction thisRoom
            newOtherRoom = unlockPathInRoom (opposite direction) otherRoom
            roomsWithThis = Map.insert (roomName newThisRoom) newThisRoom rooms
            roomsWithBoth = Map.insert (roomName newOtherRoom) newOtherRoom roomsWithThis
        in gameState {currentRoom = newThisRoom, roomStates = roomsWithBoth}


unlockPathInRoom :: Direction -> Room -> Room
unlockPathInRoom direction room =
  case Map.lookup direction (directions room) of
    Nothing -> room
    Just (otherRoomName, _) ->
      let newDirections = Map.insert direction (otherRoomName, True) (directions room)
      in room {directions = newDirections}

-- Helper function returning list of all objects that the player can interact with
allInteractables :: GameState -> [Interactable]
allInteractables gameState =
  let inventoryItems = map fst (Map.elems (inventory gameState))
      itemsInRoom = roomItems $ currentRoom gameState
      roomInteractables = interactables $ currentRoom gameState
  in inventoryItems ++ itemsInRoom ++ roomInteractables

displayHintForCurrentRoom :: GameState -> IO GameState
displayHintForCurrentRoom gameState =
  let room = currentRoom gameState
      hintText = roomHint room
  in putStrLn hintText >> return gameState

displayHintForInteractable :: String -> GameState -> IO GameState
displayHintForInteractable entityName gameState = do
  case find (\i -> name i == entityName) (allInteractables gameState) of
    Just entity -> putStrLn (hint entity)
    _ -> putStrLn "I don't see that here"
  return gameState

handleInspect :: String -> GameState -> IO GameState
handleInspect entityName gameState = do
  case find (\i -> name i == entityName) (allInteractables gameState) of
    Just entity -> (putStrLn $ description entity)
    _ -> putStrLn "I don't see that here"
  return gameState


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
