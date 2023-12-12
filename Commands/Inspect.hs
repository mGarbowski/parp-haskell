module Commands.Inspect where
import Data.List
import GameState
import Interactables

handleInspect :: String -> GameState -> IO GameState
handleInspect entityName gameState = do
  case find (\i -> name i == entityName) (allInteractables gameState) of
    Just entity -> (putStrLn $ description entity)
    _ -> putStrLn "I don't see that here"
  return gameState