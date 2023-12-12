module Hint where
import Data.List
import GameState
import Interactables
import Rooms

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