module Commands.Inspect where
import Data.List
import GameState
import Interactables
import Items
import Util
import qualified Data.Map as Map
import Data.Maybe (fromJust)

handleInspect :: String -> GameState -> IO GameState
handleInspect entityName gameState
  | entityName == name toolChest = handleToolChestInspect gameState
  | entityName == name locker = handleLockerInspect gameState
  | entityName == name compartment = handleCompartmentInspect gameState
  | otherwise = handleSimpleInspect entityName gameState


handleSimpleInspect :: String -> GameState -> IO GameState
handleSimpleInspect entityName gameState = do
  case find (\i -> name i == entityName) (allInteractables gameState) of
    Just entity -> do
      (putStrLn $ description entity)
      if entityName == "coat"
      then case Map.member (name lockerRoomKey) (inventory gameState) of
        True -> return gameState
        False -> return gameState { inventory = addItemToInventory lockerRoomKey (inventory gameState) }
      else if entityName == "broken door"
      then case Map.member (name smallKey) (inventory gameState) of
        True -> return gameState
        False -> return gameState { inventory = addItemToInventory smallKey (inventory gameState) }
      else return gameState
    _ -> do putStrLn "I don't see that here"
            return gameState


handleCompartmentInspect :: GameState -> IO GameState
handleCompartmentInspect gameState =
  case lockerCompartmentBlocked gameState of
    True -> putStrLn "The bottom compartment is locked. There is a keyhole, but where is the key?" >> return gameState
    False -> handleSimpleInspect (name compartment) gameState

handleLockerInspect :: GameState -> IO GameState
handleLockerInspect gameState =
  let lockerContents = fromJust $ Map.lookup (name locker) (containerContents gameState) in
    case coat `elem` lockerContents of
      False -> do
               putStr ("A locker for storing personal items" ++
                      "\nThe locker reveals two compartments, in the upper part, where you took the coat from and" ++
                      "\nanother compartment below it.\n")
               return gameState
      True -> handleSimpleInspect (name locker) gameState


handleToolChestInspect :: GameState -> IO GameState
handleToolChestInspect gameState = do
  let toolChestContents = fromJust $ Map.lookup (name toolChest) (containerContents gameState)
  let crowbarPresent = elem crowbar toolChestContents
  let powerCellPresent = elem powerCell toolChestContents
  putStrLn $ case (crowbarPresent, powerCellPresent) of
    (True, True) -> "The tool chest contains a crowbar and a power cell."
    (True, False) -> "The tool chest contains a crowbar."
    (False, True) -> "The tool chest contains a power cell."
    (False, False) -> "The tool chest is empty."
  return gameState
