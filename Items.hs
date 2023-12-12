module Items where

data Item = Item {
  itemName :: String,
  itemDescription :: String,
  itemHint :: String,
  itemCount :: Int
} deriving Show

lockerRoomKey :: Item
lockerRoomKey = Item {
  itemName = "locker room key",
  itemDescription = "Small, silver key. What does it open?",
  itemHint = "Try to unlock the door using this key",
  itemCount = 1
}

coat :: Item
coat = Item {
  itemName = "coat",
  itemDescription = "The lab coat stirs a sense of familiarity.\n" ++
                    "The nametag reads: \"Dr. J. Sysy.\", it triggers a cascade of hazy memories. As you run your\n" ++
                    "fingers over the fabric, flashes of experiments and research echo in your mind.\n" ++
                    "Memories start coming back to you, it is your coat!\n", -- todo
  itemHint = "Take a good look at it, maybe you will find something useful",
  itemCount = 1
}

crowbar :: Item
crowbar = Item {
  itemName = "crowbar",
  itemDescription = "A heavy, meteal crowbar painted red, the best tool for brute-force solutions",
  itemHint = "Use it to pry open the ventilation shaft",
  itemCount = 1
}

powerCell :: Item
powerCell = Item {
  itemName = "power cell",
  itemDescription = "The power cell pulses with energy, illuminating the room. It may come in handy.",
  itemHint = "It is a portable source of power",
  itemCount = 1
}

labShoes :: Item
labShoes = Item {
  itemName = "lab shoes",
  itemDescription = "A sturdy pair shoes, essential for work in hazardous environments, around chemicals. You can put them on.",
  itemHint = "Use `put on` to put on the shoes",
  itemCount = 1
}

smallKey :: Item
smallKey = Item {
  itemName = "small key",
  itemDescription = "A small key, does not match the size of the keyholes in any of the doors you have seen so far.\n" ++
                    "It must open something smaller like a...",
  itemHint = "Use it to unlock the locker compartment"
}


-- todo make item descriptions conditional as they depend on gameState
-- todo unify Item and Interactable interface, store the number of held items in the inventory or sth