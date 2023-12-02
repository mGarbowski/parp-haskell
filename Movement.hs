module Movement where
import GameState
import Rooms
import qualified Data.Map as Map

moveDirection :: String -> GameState -> IO GameState
moveDirection directionStr gameState =
  case parseDirection directionStr of
    Just direction -> case lookup direction (directions (currentRoom gameState)) of
      Just otherRoom ->
        case Map.lookup (roomName otherRoom) (roomStates gameState) of
           Just room -> return gameState {currentRoom = room}  -- set currentRoom to the updated value from gameState, todo rethink this
           _ -> return gameState
      Nothing -> do
        putStrLn "You can't go that way"
        return gameState
    Nothing -> do
      putStrLn "You can go north, south, east or west"
      return gameState