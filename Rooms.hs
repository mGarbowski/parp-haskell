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

-- Function to display the current room description and items
displayRoom :: Room -> String
displayRoom room = roomDescription room ++ "\nItems in the room: " ++ intercalate ", " (map itemName (roomItems room))

-- Function to define the initial room
lockerRoom :: Room
lockerRoom = Room {
  roomName = "Locker Room",
  roomDescription = "The locker room is dimly lit, with a faint hum of machinery in the air. Rows\n" ++
                    "of lockers fill the space, closed and orderly. However, one locker catches\n" ++
                    "your eye - slightly open, a hint of something inside. It seems like the owner\n" ++
                    "forgot to lock it properly\n" ++
                    "To the north, a heavy metal door labeled \"Security\" presents itself to you\n" ++
                    "To the east, you see another door with no labels as to where it leads\n",
  roomHint = "You can interact with the locker, go north or go east, use `examine north door` to get more information.",
  roomItems = [],
  directions = Map.fromList [
    (East, ("Corridor One", True)),
    (North, ("Security Room", False))
    ]
  }

securityRoom :: Room
securityRoom = Room {
  roomName = "Security Room",
  roomDescription = "The security room is dimly lit, the soft glow of monitors casting a blue hue on\n" ++
                    "the walls. The air is cool, and the hum of electronic equipment fills the room.\n" ++
                    "Rows of monitors display surveillance footage, capturing glimpses of different\n" ++
                    "sections of the lab. The power control switches on the central console beckon,\n" ++
                    "suggesting a pivotal role in the functionality of the facility.\n",
  roomHint = "You can interact with the control panel or go south",
  roomItems = [],
  directions = Map.fromList [
    (South, ("Locker Room", True))
  ]
}

corridorOne :: Room
corridorOne = Room {
  roomName = "Corridor One",
  roomDescription = "You find yourself in a dimly lit corridor, the ambient light casting faint shadows.\n" ++
                    "Directly ahead to the north, a door labeled \"Generator Room\" stands prominently.\n" ++
                    "Looking to the south, the corridor stretches away, its destination obscured by the dim lighting,\n" ++
                    "The western door leads to the locker room.\n",
  roomHint = "You can go west, north, south",
  roomItems = [],
  directions = Map.fromList [
    (West, ("Locker Room", True)) -- todo
  ]
}

corridorTwo :: Room
corridorTwo = Room {
  roomName = "Corridor Two",
  roomDescription = "You are further down the corridor, to the west - entrance to the experiment chamber.\n" ++
                    "The air is heavy with the weight of past scientific endeavors ...or are those toxic fumes?\n" ++
                    "To the east - a locked door guards more secrets.\n",
  roomHint = "You can go north, east, west",
  roomItems = [],
  directions = Map.fromList [
    (West, ("Locker Room", True)) -- todo
  ]
}

generatorRoom :: Room
generatorRoom = Room {
  roomName = "Generator Room",
  roomDescription = "As you explore the generator room, the dormant generator at the center stands in\n" ++
                    "stark contrast to the adjacent ventilation shaft. The idle turbines offer a\n" ++
                    "glimpse into the halted functionality of the lab's power source.\n" ++
                    "The ventilation shaft on the southern wall,\n" ++
                    "though vibrating with a faint breeze, seems large enough for someone to\n" ++
                    "fit through.\n",
  roomHint = "You can interact with generator, vent or go south",
  roomItems = [],
  directions = Map.fromList [] -- todo
}

exitRoom :: Room
exitRoom = Room {
  roomName = "Exit Room",
  roomDescription = "As you're crawling out of the ventilation shaft, you notice an elevator.\n" ++
                    "Finally, a way out of this maze!\n",
  roomHint = "You can interact with vent and elevator",
  roomItems = [],
  directions = Map.fromList [] -- todo
}

experimentRoom :: Room
experimentRoom = Room {
  roomName = "Experiment Room",
  roomDescription = "You are in the experiment room. The floor is covered in a sticky, toxic sludge.\n" ++
                    "On the other side of the room, you see a small metal tool chest.\n" ++
                    "If you were only able to go through the sludge,\n" ++
                    "you could see what's inside. The door was blown open by the failure of the experiment, it seems.\n" ++
                    "The broken door is laying next to where you stand.\n",
  roomHint = "You need to acquire some protective gear to walk through the toxic sludge.\n" ++
             "You can interact with tool chest and broken door\n",
  roomItems = [],
  directions = Map.fromList [] -- todo
}

computerRoom :: Room
computerRoom = Room {
  roomName = "Computer Room",
  roomDescription = "As you step into the room bathed in soft light, you notice a solitary desk at\n" ++
                    "its center. On the desk rests a computer, its screen silent and\n" ++
                    "dark. The absence of the usual hum of electronic activity adds an eerie calm\n" ++
                    "to the room, leaving the space seemingly dormant.\n" ++
                    "To the west there is a door locked from this side\n",
  roomHint = "You can interact with: computer, desk, vent or try the door to the west",
  roomItems = [],
  directions = Map.fromList [] -- todo
}

-- todo take descriptions outside of Room objects as they depend on GameState in the prolog version