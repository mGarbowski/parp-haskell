module Movement where
import GameState
import Rooms
import qualified Data.Map as Map

moveDirection :: String -> GameState -> IO GameState
moveDirection directionStr gameState =
  case parseDirection directionStr of
    Just direction -> case lookup direction (directions (currentRoom gameState)) of
      Just otherRoomName ->
        case Map.lookup otherRoomName (roomStates gameState) of
           Just otherRoom -> return gameState {currentRoom = otherRoom}
           _ -> return gameState
      Nothing -> do
        putStrLn "You can't go that way"
        return gameState
    Nothing -> do
      putStrLn "You can go north, south, east or west"
      return gameState