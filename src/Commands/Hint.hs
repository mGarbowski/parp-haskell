module Commands.Hint where
import Data.List
import GameState
import Interactables
import Rooms

displayHintForCurrentRoom :: GameState -> IO GameState
displayHintForCurrentRoom gameState = do
  let room = currentRoom gameState
  let hintText = roomHint room
  putStrLn hintText
  putStrLn ("Objects in the room: " ++ intercalate ", " (map name (roomItems room ++ interactables room)))
  return gameState

displayHintForInteractable :: String -> GameState -> IO GameState
displayHintForInteractable entityName gameState = do
  case find (\i -> name i == entityName) (allInteractables gameState) of
    Just entity -> putStrLn (hint entity)
    _ -> putStrLn "I don't see that here"
  return gameState