module Interactables where

-- for all objects that the player can interact with and are not pickable objects
-- assigned to rooms to ensure they can only be interacted with if the player is in the same room
data Interactable = Interactable {
  name :: String,
  description :: String,
  hint :: String
} deriving (Show, Eq)

-- todo
-- tool chest ?
-- locker ?
-- locker compartment ?

controlPanel :: Interactable
controlPanel = Interactable {
  name = "control panel",
  description = "The central console stands prominently, adorned with an array of power control\n" ++
                "switches. Their sleek design and illuminated indicators add a touch of modernity\n" ++
                "to the room. Labels beside each switch hint at their specific functions. The\n" ++
                "control panel exudes an air of authority, silently overseeing the lab's intricate\n" ++
                "systems from its place in the security room.\n" ++
                "One switch catches your eye - it's the elevator power supply\n" ++
                "This is what you have been looking for!\n",
  hint = "You can use it to `power on` something"
}

ventEntrance :: Interactable
ventEntrance = Interactable {
  name = "vent",
  description = "A big, rectangular ventilation shaft large enough for a human to crawl inside",
  hint = "You can `enter` the vent"
}

computer :: Interactable
computer = Interactable {
  name = "computer",
  description = "On the desk, a computer sits dormant. Its screen is powered off, casting a\n" ++
                "dark reflection in the quiet room. The keyboard awaits your interaction,\n" ++
                "could its hard drives have some answers for you?\n",
  hint = "You can use it as a decorative piece or `power on computer`. to make better use of it."
}

generator :: Interactable
generator = Interactable {
  name = "generator",
  description = "The emergency power generator looms with a single glowing cell within, flanked by two conspicuous empty slots,\n" ++
                "rendering the machine silent and incomplete, waiting for its missing components.\n" ++
                "In order to restore electricity in the lab, the two remaining cells have to be found.\n", -- todo
  hint = "Use `power on generator` to power it on"
}

brokenDoor :: Interactable
brokenDoor = Interactable {
  name = "broken door",
  description = "You inspect the door closely and decide to flip it over.\n" ++
                "What a surprise! Somebody must've put a key into the keyhole.\n" ++
                "The key is bent, but it is attached to a keychain, on which there is another key. What could it unlock?", -- todo
  hint = "Maybe taking a closer look at it will reveal something?"
}

elevator :: Interactable
elevator = Interactable {
  name = "elevator",
  description = "Approaching the elevator, you find it disabled, a security override in effect.\n" ++
                "A prominent display blinks with a warning, indicating that access is restricted\n" ++
                "and the elevator is currently non-operational. The usual hum of its machinery\n" ++
                "is replaced by an eerie silence. Looks like the security staff can turn it on/off during emergencies.\n", --todo
  hint = "To go inside use `enter elevator`"
}

desk :: Interactable
desk = Interactable {
  name = "desk",
  description = "The desk in the room is simple yet functional. It holds a powered-off computer\n" ++
                "at its center. The smooth surface suggests a place for work or study, while\n" ++
                "the surrounding space remains uncluttered, you bend to see what's underneath it\n" ++
                "There is a sticky note! It says \"pass: rot13(XvetvfJnygre)\"\n" ++
                "What's rot13? The computer nerds from the lab wouldn't just leave a password in plain text for everyone to see\n",
  hint = "just google it"
}

keypad :: Interactable
keypad = Interactable {
  name = "keypad",
  description = "A modern keypad, mounted beside the security room door, stands as the gateway to the lab.\n" ++
                "It features a digital touchscreen with a numeric grid and softly backlit keys.\n" ++
                "To enter, you must input the correct 6-digit code.",
  hint = "Use `enter keypad <code>` to enter a code\n" ++
         "Only 6 digits? That's only like 999 999 options, you can just brute force it...\n" ++
         "...or look around for the code, it must be *somewhere*\n"
}

toolChest :: Interactable
toolChest = Interactable {
  name = "tool chest",
  description = "Inside there is a crowbar and a power cell",
  hint = "Wearing the shoes, you can cross the toxic puddle and reach into the tool chest"
}

locker :: Interactable
locker = Interactable {
  name = "locker",
  description = "A locker for storing personal items" ++
                "\nThe locker reveals two compartments, in the upper part, a lab coat hangs neatly on a hanger" ++
                "\nBelow there is another locker_compartment.\n",
  hint = "Look what's inside"
}