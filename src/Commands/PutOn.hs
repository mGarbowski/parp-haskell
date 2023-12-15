module Commands.PutOn where
import qualified Data.Map as Map
import GameState
import Rooms
import Items
import Interactables

handlePutOnShoes :: GameState -> IO GameState
handlePutOnShoes gameState =
  case Map.member (name labShoes) (inventory gameState) of
    False -> putStrLn "I don't see that here" >> return gameState
    True -> do
      let newInventory = Map.delete (name labShoes) (inventory gameState)
      let newRoomStates = Map.insert (roomName experimentRoom) experimentRoomWearingShoes (roomStates gameState)
      let updatedGameState = gameState {inventory = newInventory, roomStates = newRoomStates}
      let withCurrentRoom = updateCurrentRoom updatedGameState
      putStrLn "The boots are really heavy and feel solid. You put them on."
      return withCurrentRoom
