module GameState where
import Rooms

data GameState = GameState { currentRoom :: Room, inventory :: [String] }

-- Function to initialize the game state
initialGameState :: GameState
initialGameState = GameState
  { currentRoom = initialRoom
  , inventory = []
  }