module Rooms where
import Data.List

data Room = Room { roomName :: String, roomDescription :: String, roomItems :: [String] }

-- Function to define the initial room
initialRoom :: Room
initialRoom = Room
  { roomName = "Starting Room"
  , roomDescription = "You find yourself in a dimly lit room."
  , roomItems = ["Key", "Note"]
  }


-- Function to display the current room description and items
displayRoom :: Room -> String
displayRoom room = roomDescription room ++ "\nItems in the room: " ++ intercalate ", " (roomItems room)