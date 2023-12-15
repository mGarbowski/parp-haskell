module Commands.Look where
import Interactables
import GameState
import Rooms
import Control.Monad.IO.Class (liftIO)

handleLook :: GameState -> IO GameState
handleLook gameState = do
  liftIO (putStrLn $ roomDescription (currentRoom gameState))
  return gameState
