module Commands.Inspect where
import Data.List
import GameState
import Interactables
import Items
import Commands.Inventory

handleInspect :: String -> GameState -> IO GameState
handleInspect entityName gameState = do
  case find (\i -> name i == entityName) (allInteractables gameState) of
    Just entity -> do
      (putStrLn $ description entity)
      if entityName == "coat"
      then return gameState { inventory = addItemToInventory lockerRoomKey (inventory gameState) }
      else if entityName == "broken door"
      then return gameState { inventory = addItemToInventory lockerRoomKey (inventory gameState) }
      else return gameState
    _ -> putStrLn "I don't see that here" >> return gameState