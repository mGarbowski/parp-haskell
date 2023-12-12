module Rooms where
import Data.List
import qualified Data.Map as Map
import Items

data Direction = North | South | West | East deriving (Eq, Ord, Show)

parseDirection :: String -> Maybe Direction
parseDirection "north" = Just North
parseDirection "south" = Just South
parseDirection "west"  = Just West
parseDirection "east"  = Just East
parseDirection _ = Nothing

data Room = Room {
  roomName :: String,
  roomDescription :: String,
  roomHint :: String,
  roomItems :: [Item],
  directions :: Map.Map Direction (String, Bool) -- mapping direction to adjacent room's name and Bool whether path is unlocked
  }
  deriving Show

-- Function to define the initial room
initialRoom :: Room
initialRoom = Room {
  roomName = "Starting Room",
  roomDescription = "You find yourself in a dimly lit room.",
  roomHint = "Try to look around and take some items.",
  roomItems = [key, note],
  directions = Map.fromList [
    (North, (roomName secondRoom, True))
    ]
  }

secondRoom :: Room
secondRoom = Room {
  roomName = "Second room",
  roomDescription = "You moved to another room.",
  roomHint = "Try to go back.",
  roomItems = [],
  directions = Map.fromList [
    (South, (roomName initialRoom, False))
    ]
  }


-- Function to display the current room description and items
displayRoom :: Room -> String
displayRoom room = roomDescription room ++ "\nItems in the room: " ++ intercalate ", " (map itemName (roomItems room))