module Items where

data Item = Item {
  name :: String,
  description :: String,
  hint :: String,
  count :: Int
} deriving Show

lockerRoomKey :: Item
lockerRoomKey = Item {
  name = "locker room key",
  description = "Small, silver key. What does it open?",
  hint = "Try to unlock the door using this key",
  count = 1
}

coat :: Item
coat = Item {
  name = "coat",
  description = "The lab coat stirs a sense of familiarity.\n" ++
                    "The nametag reads: \"Dr. J. Sysy.\", it triggers a cascade of hazy memories. As you run your\n" ++
                    "fingers over the fabric, flashes of experiments and research echo in your mind.\n" ++
                    "Memories start coming back to you, it is your coat!\n", -- todo
  hint = "Take a good look at it, maybe you will find something useful",
  count = 1
}

crowbar :: Item
crowbar = Item {
  name = "crowbar",
  description = "A heavy, meteal crowbar painted red, the best tool for brute-force solutions",
  hint = "Use it to pry open the ventilation shaft",
  count = 1
}

powerCell :: Item
powerCell = Item {
  name = "power cell",
  description = "The power cell pulses with energy, illuminating the room. It may come in handy.",
  hint = "It is a portable source of power",
  count = 1
}

labShoes :: Item
labShoes = Item {
  name = "lab shoes",
  description = "A sturdy pair shoes, essential for work in hazardous environments, around chemicals. You can put them on.",
  hint = "Use `put on` to put on the shoes",
  count = 1
}

smallKey :: Item
smallKey = Item {
  name = "small key",
  description = "A small key, does not match the size of the keyholes in any of the doors you have seen so far.\n" ++
                    "It must open something smaller like a...",
  hint = "Use it to unlock the locker compartment",
  count = 1
}


-- todo make item descriptions conditional as they depend on gameState
-- todo unify Item and Interactable interface, store the number of held items in the inventory or sth