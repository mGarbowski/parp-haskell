module GameState where
import Rooms
import Items
import qualified Data.Map as Map

data GameState = GameState {
  currentRoom :: Room,
  inventory :: [Item],
  roomStates :: Map.Map String Room
  }

-- Function to initialize the game state
initialGameState :: GameState
initialGameState = GameState {
  currentRoom = initialRoom,
  inventory = [],
  roomStates = Map.fromList [
    (roomName initialRoom, initialRoom),
    (roomName secondRoom, secondRoom)
    ]
  }