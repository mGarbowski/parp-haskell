module Commands.Unlock where
import qualified Data.Map as Map
import GameState
import Rooms
import Items
import Interactables

handleUnlock :: String -> GameState -> IO GameState
-- unlock vent if possible
handleUnlock "vent" gameState =
  if roomName (currentRoom gameState) /= roomName generatorRoom
  then putStrLn "I don't see that here" >> return gameState
  else
    case ventBlocked gameState of
      False -> putStrLn "You've already opened the vent!" >> return gameState
      True -> if Map.member (name crowbar) (inventory gameState)
              then putStrLn ("You slide the crowbar between the vent door and its door frame\n" ++
                   "and push with a lot of force. The vent swings open!")
                   >> return gameState {ventBlocked = False}
              else putStrLn ("The vent is closed shut and the door doesn't seem to budge.\n" ++
                  "Maybe using some tool would help?")
                >> return gameState

-- unlock compartment if possible
handleUnlock "compartment" gameState =
  if roomName (currentRoom gameState) /= roomName lockerRoom
    then putStrLn "I don't see that here" >> return gameState
    else
      if Map.member (name smallKey) (inventory gameState)
      then do
        putStrLn("Unlocked!")
        return $ gameState { lockerCompartmentBlocked = False}
      else
        putStrLn "Can't unlock it. Perhaps you need something to unlock it with?" >> return gameState


-- unlock a door in a given direction if possible
handleUnlock directionStr gameState =
  case parseDirection directionStr of
    Nothing -> return gameState
    Just direction ->
      if canUnlock direction (roomName $ currentRoom gameState) gameState
      then putStrLn "Door unlocked" >> return (unlockPath direction gameState)
      else putStrLn "You can't open it" >> return gameState


-- direction, current room name, game state, can player unlock door in `direction` from current room
canUnlock :: Direction -> String -> GameState -> Bool
canUnlock East "Locker Room" gameState = Map.member (name lockerRoomKey) (inventory gameState)
canUnlock North "Locker Room" gameState = keycodeEntered gameState
canUnlock West "Computer Room" _ = True
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


-- helper function, unlocks the path to another room once the player has unlocked the door
unlockPathInRoom :: Direction -> Room -> Room
unlockPathInRoom direction room =
  case Map.lookup direction (directions room) of
    Nothing -> room
    Just (otherRoomName, _) ->
      let newDirections = Map.insert direction (otherRoomName, True) (directions room)
      in room {directions = newDirections}