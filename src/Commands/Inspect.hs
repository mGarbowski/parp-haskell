module Commands.Inspect where
import Data.List
import GameState
import Interactables
import Items
import Util
import Container
import qualified Data.Map as Map
import Data.Maybe (fromJust)

handleInspect :: String -> GameState -> IO GameState
handleInspect entityName gameState
  {- the toolChest, locker, compartment, broken door and coat are items with dynamic descriptions so they
     require special cases -}
  | entityName == name toolChest = handleToolChestInspect gameState
  | entityName == name locker = handleLockerInspect gameState
  | entityName == name compartment = handleCompartmentInspect gameState
  | entityName == name coat = handleSpecialInspect coat gameState
  | entityName == name brokenDoor = handleSpecialInspect brokenDoor gameState
  -- basic inspect operation
  | otherwise = handleSimpleInspect entityName gameState


handleSimpleInspect :: String -> GameState -> IO GameState
handleSimpleInspect entityName gameState = do
  case find (\i -> name i == entityName) (allInteractables gameState) of
    Just entity -> do
      (putStrLn $ description entity)
    _ -> do putStrLn "I don't see that here"
  return gameState

-- this takes care of inspecting the items which yield another item at the first interaction
handleSpecialInspect :: Interactable -> GameState -> IO GameState
handleSpecialInspect entity gameState =
  let entityName = name entity in do
    putStrLn $ description entity
    if entityName == name coat
    -- check if the lockerRoomKey was already collected from the coat and add extra text if not
    then case Map.member (name lockerRoomKey) (inventory gameState) of
            True -> return gameState
            False -> do putStrLn "Instinctively you check your pockets. You feel a small, cold object - a key"
                        return gameState { inventory = addItemToInventory lockerRoomKey (inventory gameState) }
    else if entityName == name brokenDoor
    -- similar case with broken door
         then case Map.member (name smallKey) (inventory gameState) of
           True -> return gameState
           False -> do putStr ("You inspect the door closely and decide to flip it over.\n" ++
                               "What a surprise! Somebody must've put a key into the keyhole.\n" ++
                               "The key is bent, but it is attached to a keychain, on which there is another key.\n" ++
                               "What could it unlock?\n")
                       return gameState { inventory = addItemToInventory smallKey (inventory gameState) }
         else return gameState

-- the locker compartments gets a different description based on whether it is locked or not
handleCompartmentInspect :: GameState -> IO GameState
handleCompartmentInspect gameState =
  case lockerCompartmentBlocked gameState of
    True -> putStrLn "The bottom compartment is locked. There is a keyhole, but where is the key?" >> return gameState
    False -> handleSimpleInspect (name compartment) gameState

-- if the coat has been taken from the locker, don't include it in the description of the locker
handleLockerInspect :: GameState -> IO GameState
handleLockerInspect gameState =
  let lockerContents = fromJust $ Map.lookup (name locker) (containerContents gameState) in
    case coat `elem` lockerContents of
      False -> do
               putStr ("A locker for storing personal items" ++
                      "\nThe locker reveals two compartments, in the upper part, where you took the coat from and" ++
                      "\nanother compartment below it.")
               return gameState
      True -> handleSimpleInspect (name locker) gameState


-- the tool chest gets different descriptions based on the items it contains
handleToolChestInspect :: GameState -> IO GameState
handleToolChestInspect gameState = do
  isAvailable <- isContainerAvaialable (name powerCell) gameState
  case isAvailable of
    False -> return gameState
    True -> do
              let toolChestContents = fromJust $ Map.lookup (name toolChest) (containerContents gameState)
              let crowbarPresent = elem crowbar toolChestContents
              let powerCellPresent = elem powerCell toolChestContents
              putStrLn $ case (crowbarPresent, powerCellPresent) of
                (True, True) -> "The tool chest contains a crowbar and a power cell."
                (True, False) -> "The tool chest contains a crowbar."
                (False, True) -> "The tool chest contains a power cell."
                (False, False) -> "The tool chest is empty."
              return gameState
