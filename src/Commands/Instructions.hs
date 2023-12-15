module Commands.Instructions where

-- Function to display available instructions
displayInstructions :: IO ()
displayInstructions = do
  putStrLn "Available Instructions:"
  putStrLn "  - look: View the current room"
  putStrLn "  - inspect X: Inspect an item in the room"
  putStrLn "  - take X: Take an item from the room and add it to your inventory"
  putStrLn "  - inventory: View your current inventory"
  putStrLn "  - go DIRECTION: move to another room to the north, south, east or west"
  putStrLn "  - instructions: View available instructions\n"
  -- todo