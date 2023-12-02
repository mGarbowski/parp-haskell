module Items where

data Item = Item {
  itemName :: String,
  itemHint :: String,
  itemCount :: Int,
  itemDescription :: String
}

key :: Item
key = Item {
  itemName = "Key",
  itemHint = "you can use this to open doors",
  itemCount = 1,
  itemDescription = "A small, metal key"
}

note :: Item
note = Item {
  itemName = "Note",
  itemHint = "inspect this note",
  itemCount = 1,
  itemDescription = "The note says: 'They security key is 123456'"
}