module GameState where
import Rooms
import Items

data GameState = GameState { currentRoom :: Room, inventory :: [Item] }

-- Function to initialize the game state
initialGameState :: GameState
initialGameState = GameState {
  currentRoom = initialRoom,
  inventory = []
  }