module Movement where
import GameState
import Rooms
import qualified Data.Map as Map

moveDirection :: String -> GameState -> IO GameState
moveDirection directionStr gameState = case parseDirection directionStr of
  Just direction ->
    case Map.lookup direction (directions (currentRoom gameState)) of
      Just (otherRoomName, False) -> handlePathLocked
      Just (otherRoomName, True) -> tryMove otherRoomName
      Nothing -> handleNoPathInThisDirection
  Nothing -> handleInvalidDirection
  where
    tryMove otherRoomName = case Map.lookup otherRoomName (roomStates gameState) of
      Just otherRoom -> return $ gameState { currentRoom = otherRoom }
      _ -> return gameState
    handleInvalidDirection = putStrLn "You can go north, south, east, or west" >> return gameState
    handleNoPathInThisDirection = putStrLn "You can't go that way" >> return gameState
    handlePathLocked = putStrLn "This path is locked" >> return gameState
