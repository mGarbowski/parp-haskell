module Rooms where
import Data.List

data Room = Room {
  roomName :: String,
  roomDescription :: String,
  roomHint :: String,
  roomItems :: [String]
  }

-- Function to define the initial room
initialRoom :: Room
initialRoom = Room {
  roomName = "Starting Room",
  roomDescription = "You find yourself in a dimly lit room.",
  roomHint = "Try to look around and take some items.",
  roomItems = ["Key", "Note"]
  }


-- Function to display the current room description and items
displayRoom :: Room -> String
displayRoom room = roomDescription room ++ "\nItems in the room: " ++ intercalate ", " (roomItems room)